###
# Processing results from fitted models
# Zane Billings
# Started Sat Nov 19 20:10:40 2022
# In this script, I will process the predictions and metrics from the fitted
# models so that they can be presented nicely. This will mean
# A) Gathering all the model predictions together.
# B) Calculating AUROCC values for the tree and score models.
# C) Gathering all the AUROCC values together.
# D) Computing stratum-specific likelihood ratios and percentages for the
#    supplementary materials.
###

# Setup ####

# Require dependencies
box::use(
	readr,
	here,
	purrr,
	dplyr,
	yardstick
)

# Load function for stratum-specific statistics
source(here::here("R", "Common-Functions", "Stratum-Specific-Stats.R"))

# Processing predictions ####

## Patient predictions ####

predictions_pt <-
	purrr::map(
		.x = list.files(
			path = here::here("Results", "Data", "Pt"),
			pattern = "Risk-Preds-.*\\.Rds",
			recursive = TRUE,
			full.names = TRUE
		),
		.f = readr::read_rds
	) |>
	dplyr::bind_rows() |>
	dplyr::mutate(rater = "patient")

## Clinician predictions ####
predictions_cl <-
	purrr::map(
		.x = list.files(
			path = here::here("Results", "Data", "Cl"),
			pattern = "Risk-Preds-.*\\.Rds",
			recursive = TRUE,
			full.names = TRUE
		),
		.f = readr::read_rds
	) |>
	dplyr::bind_rows() |>
	dplyr::mutate(rater = "clinician")

## Bind predictions and export ####
predictions_all <-
	dplyr::bind_rows(
		predictions_pt,
		predictions_cl
	)

readr::write_rds(
	predictions_all,
	here::here("Results", "Data", "Model-Predictions.rds")
)

# Processing AUROCCs ####

## Tree and Score AUROCC calculations ####
# One of the models has no predictions of a certain value so we need to
# suppress the warning telling us about that
suppressWarnings(
ts_aucs <-
	predictions_all |>
	dplyr::filter(model_group != "machine learning") |>
	tidyr::nest(data = c(unique_visit, diagnosis, risk_pred, risk_group)) |>
	dplyr::mutate(
		auc = purrr::map(
			.x = data,
			.f = yardstick::roc_auc,
			truth = diagnosis,
			risk_pred,
			event_level = "second"
		)
	) |>
	tidyr::unnest(auc) |>
	dplyr::transmute(
		rater, sample, model, model_group,
		auc = .estimate,
		se = NA_real_
	)
)

## Joining AUROCCs together
ml_aucs <-
	dplyr::bind_rows(
		"patient" =
			readr::read_rds(
				here::here("Results", "Data", "Pt", "ML-Models", "AUC-ML.Rds")
			),
		"clinician" =
			readr::read_rds(
				here::here("Results", "Data", "Cl", "ML-Models", "AUC-ML.Rds")
			),
		.id = "rater"
	) |>
	dplyr::mutate(
		model = gsub("recipe_", "", model),
		model_group = "machine learning"
	)

aucs_all <- dplyr::bind_rows(ts_aucs, ml_aucs)

aucs_all |>
	ggplot() +
	aes(x = auc, xmin = auc -  1.96 * se, xmax = auc + 1.96 * se,
			y = forcats::fct_rev(model), shape = sample, color = sample) +
	geom_pointrange() +
	facet_grid(rows = vars(model_group), cols = vars(rater), scales = "free_y") +
	scale_x_continuous(limits = c(0.5, 1.0)) +
	scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
	labs(
		x = "AUROCC",
		y = NULL,
		color = NULL,
		shape = NULL
	) +
	zlib::theme_ms() +
	theme(strip.text = element_text(margin = margin(b = 2, t = 2, r = 2, l = 2)))


readr::write_rds(
	aucs_all,
	here::here("Results", "Data", "Model-AUCs.rds")
)

# Stratum-specific statistics ####
strata_res <-
	predictions_all |>
	# Recode diagnosis for stratum-specific functions
	dplyr::mutate(
		diag2 = (diagnosis == "Influenza +")
	) |>
	# Nest data for easy mapped computation of stratum-specific statistics
	tidyr::nest(data = c(unique_visit, diagnosis, diag2, risk_group,
											 risk_pred)) |>
	# Compute the statistics and process the outputs here
	dplyr::mutate(
		stats = purrr::map(data, ~stratum_specific_stats(.x[[3]], .x[[4]])),
		counts = purrr::map(data, ~stratum_specific_counts(.x[[3]], .x[[4]])),
		out = purrr::map2(
			stats, counts,
			~dplyr::left_join(.x, .y, by = "group")
		)
	) |>
	# Clean up the results
	tidyr::unnest(out) |>
	dplyr::select(-data, -stats, -counts)

readr::write_rds(
	strata_res,
	here::here("Results", "Data", "Stratum-Specific-Stats.Rds")
)

# END OF FILE ####
