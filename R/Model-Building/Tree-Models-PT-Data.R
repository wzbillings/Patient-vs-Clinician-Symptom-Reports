###
# Tree models
# Zane Billings
# Created Wed Nov  9 12:19:56 2022
# This script will fit the manual tree-type models (not tuned via cross
# validation). For more information, see the Model-Building.qmd document.
###

# Setup ####

# Set PRNG seed
set.seed(370)

# List required dependencies

box::use(
	here,
	readr,
	dplyr,
	snakecase,
	rpart,
	FFTrees,
	C50,
	partykit
)

# Load the data

dl <- readr::read_rds(here::here("Data", "Clean-Data", "model-data.Rds"))
pcr_der <- dl[["derivation"]]
pcr_val <- dl[["validation"]]

# Data processing ####
# We have to do some futzing to make the tree functions work nicely, they
# aren't as user friendly as glm.
tree_data_der <-
	pcr_der |>
	dplyr::mutate(
		diagnosis = factor(
			diagnosis,
			levels = c("negative", "positive"),
			labels = c("Influenza -", "Influenza +")
		),
		dplyr::across(
			-c(unique_visit, diagnosis),
			\(x) factor(
				x,
				levels = c(0, 1),
				labels = c("Absent", "Present")
			)
		)
	) |>
	dplyr::rename_with(
		.cols = -c(unique_visit, diagnosis),
		.fn = snakecase::to_sentence_case
	) |>
	dplyr::rename("Chills/Sweats" = "Chills sweats")

# Fit the tree models ####
tree_formula <-
	tree_data_der |>
	dplyr::select(-diagnosis, -`Dx num`, -unique_visit) |>
	names() |>
	purrr::map_chr(\(x) paste0('`', x, '`')) |>
	reformulate(response = "diagnosis")

## rpart / CART algorithm ####
fit_cart <-
	rpart::rpart(
		formula = tree_formula,
		data = tree_data_der,
		method = "class"
	)

## FFT algorithm ####
fit_fft <-
	FFTrees::FFTrees(
		formula = tree_formula,
		data = tree_data_der,
		goal = "bacc",
		goal.chase = "bacc",
		goal.threshold = "bacc",
		do.comp = FALSE
	)

## C5.0 algorithm ####
fit_c50 <-
	C50::C5.0(
		formula = tree_formula,
		data = tree_data_der,
		trials = 1,
		control = C50::C5.0Control(
			subset = TRUE,
			winnow = TRUE,
			seed = 370
		)
	)

## Conditional inference tree algorithm ####
fit_ctree <-
	partykit::ctree(
		formula = tree_formula,
		data = tree_data_der,
		control = partykit::ctree_control(
			# 1 - p-value for splitting to be allowed
			mincriterion = 0.80
		)
	)

## Save tree models to disk ####
readr::write_rds(
	list(fit_cart, fit_fft, fit_c50, fit_ctree),
	here::here("Results", "Data", "Pt", "Tree-Models",
						 "Tree-Models.Rds")
)

# Get risk predictions for each individual ####
tree_pred_risks <-
	dplyr::bind_rows(
		"derivation" = pcr_der,
		"validation" = pcr_val,
		.id = "sample"
	) |>
	# Repeat the previous data cleaning step to ensure predictions
	# can be done with the models
	dplyr::mutate(
		diagnosis = factor(
			diagnosis,
			levels = c("negative", "positive"),
			labels = c("Influenza -", "Influenza +")
		),
		dplyr::across(
			-c(unique_visit, diagnosis, sample),
			\(x) factor(
				x,
				levels = c(0, 1),
				labels = c("Absent", "Present")
			)
		),
		sample = factor(
			sample,
			levels = c("derivation", "validation"),
			labels = c("Derivation group", "Validation group")
		)
	) |>
	dplyr::rename_with(
		.cols = -c(unique_visit, diagnosis, sample),
		.fn = snakecase::to_sentence_case
	) |>
	dplyr::rename("Chills/Sweats" = "Chills sweats") |>
	# Nesting to get predictions correctly
	tidyr::nest(symp = -sample) |>
	dplyr::mutate(
		# rpart (AKA cart)
		tree_cart = purrr::map(
			symp,
			\(x) predict(fit_cart, newdata = x, type = "prob")[, 2]
		),
		# fft
		tree_fft = purrr::map(
			symp,
			# The FFTrees package specifically gives a useless warning that it causes
			# itself by doing something in a weird way. We can ignore it.
			\(x) predict(
				fit_fft,
				newdata = x |>
					# We have to make the diagnosis variable a logical for this for some
					# reason even though it lets us fit the model without doing that
					dplyr::mutate(diagnosis = (diagnosis == "Influenza +")) |>
					# Suppress the annoying FFTrees warning in a way that dplyr likes
					dplyr::select(-c(unique_visit, `Dx num`)),
				type = "prob"
			)[["prob_1"]]
		),
		# c5.0
		tree_c50 = purrr::map(
			symp,
			\(x) predict(
				fit_c50,
				newdata = x,
				trials = 1,
				type = "prob")[, "Influenza +"]
		),
		# ctree
		tree_ctree = purrr::map(
			symp,
			\(x) predict(
				fit_ctree,
				newdata = x,
				type = "prob")[, 2]
		)
	) |>
	tidyr::unnest(tidyselect::starts_with("tree_") | symp) |>
	dplyr::select(sample, unique_visit, diagnosis, starts_with("tree_"))

# Clean up and export predictions ####
risk_data_to_merge <-
	tree_pred_risks |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("tree_"),
		names_to = "model",
		values_to = "risk_pred"
	) |>
	dplyr::relocate(
		model,
		.before = diagnosis
	) |>
	dplyr::mutate(
		model = dplyr::recode(
			model,
			"tree_cart" = "CART",
			"tree_fft" = "FFT",
			"tree_c50" = "C5.0",
			"tree_ctree" = "Conditional inference"
		),
		risk_group = dplyr::case_when(
			risk_pred <= 0.10 ~ "low",
			risk_pred <= 0.50 ~ "med",
			risk_pred >  0.50 ~ "high"
		),
		risk_group = factor(
			risk_group,
			levels = c("low", "med", "high"),
			labels = c("Low", "Moderate", "High"),
			ordered = TRUE
		),
		model_group = "decision tree"
	)

readr::write_rds(
	risk_data_to_merge,
	file = here::here("Results", "Data", "Pt", "Tree-Models",
										"Risk-Preds-Trees.Rds")
)

# END OF FILE ####
