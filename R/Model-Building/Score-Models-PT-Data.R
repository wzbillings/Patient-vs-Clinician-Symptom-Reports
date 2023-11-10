###
# START OF FILE ####
# Score Models
# Zane Billings
# Created 2022-09-29
# Updated Wed Nov 09 12:08:58 2022
# This script will build and process the linear regression-based score-type
# models for predicting flu risk. For information on how the models were
# developed in the first place, see the Model-Building.qmd document.
###

# Setup ####

# List required dependencies

box::use(
	here,
	readr,
	glmnet
)

# Load the data

dl <- readr::read_rds(here::here("Data", "Clean-Data", "model-data.Rds"))
pcr_der <- dl[["derivation"]]
pcr_val <- dl[["validation"]]

# Fitting models ####

## Creating and exporting GLM model objects ####

### LASSO model ####
# First do variable selection with glmnet to get a LASSO score model as an
# alternative to the stepwise model that Ebell et al applied.

# The first step is to make a numeric matrix of predictors for glmnet
pred_matrix <-
	pcr_der |>
	dplyr::select(-unique_visit, -diagnosis, -dx_num) |>
	as.matrix()

# Get the optimal penalty parameter via cross validation
glmnet_cv <-
	glmnet::cv.glmnet(
		x = pred_matrix,
		y = pcr_der[["dx_num"]],
		family = "binomial"
	)

# Fit the glmnet model using the best penalty within 1 SE of the error
glmnet_model <-
	glmnet::glmnet(
		x = pred_matrix,
		y = pcr_der[["dx_num"]],
		family = "binomial",
		lambda = glmnet_cv$lambda.1se
	)

# Get the list of coefficients to include (the active set of the LASSO model)
lasso_score_vars <-
	# Extract only the nonzero coefficients from the glmnet model
	glmnet_model |>
	broom:::tidy.glmnet(return_zeros = FALSE) |>
	# Remove the intercept, we don't need to put that in the formula
	dplyr::filter(term != "(Intercept)") |>
	# Get all of the names of the included predictors, and then use those to
	# construct the formula object for the relaxed model
	dplyr::pull(term)

# Formulae to use for model fitting
fmla_list <-
	list(
		# A priori model
		a_priori = c("cough", "subjective_fever", "acute_onset", "chills_sweats",
								 "myalgia"),
		# LASSO model
		lasso = lasso_score_vars,
		# Same predictors as Ebell score 3
		ebell = c("acute_onset", "myalgia", "chills_sweats",
							"cough:subjective_fever"),
		# Unweighted heuristic models (interaction only)
		cf = c("cough:subjective_fever"),
		cfa = c("cough:subjective_fever:acute_onset"),
		cfm = c("cough:subjective_fever:myalgia"),
		# Unweighted lasso model
		lasso = paste0(lasso_score_vars, collapse = ":"),
		# Weighted heuristic models
		cf_w = c("cough", "subjective_fever"),
		cfa_w = c("cough", "subjective_fever", "acute_onset"),
		cfm_w = c("cough", "subjective_fever", "myalgia")
	) |>
	purrr::map(.f = reformulate, response = "dx_num", intercept = TRUE)

# Fit all of the models
glm_list <-
	purrr::map(
		.x = fmla_list,
		.f = glm,
		family = "binomial",
		data = pcr_der
	)

# Give the models nicer names
names(glm_list) <-
	c(
		"A priori symptoms",
		"LASSO",
		"Ebell flu score symptoms",
		"CF (unweighted)",
		"CFA (unweighted)",
		"CFM (unweighted)",
		"LASSO variables (unweighted)",
		"CF (weighted)",
		"CFA (weighted)",
		"CFM (weighted)"
	)

# Save glm list to disk
glm_list |>
	readr::write_rds(
		here::here("Results", "Data", "Pt", "Score-Models",
							 "Score-GLMs.Rds")
	)

## Getting scores for each model/participant ####

# First get the model coefficients and round to get point scores
score_model_coefs <-
	purrr::map(
		glm_list,
		\(model) model |>
			# Get the coefficients from the model with CIs
			broom:::tidy.glm(conf.int = TRUE, conf.level = 0.95) |>
			# Remove the intercept since it isn't used in the score
			dplyr::filter(term != "(Intercept)") |>
			# Round coefficients to nearest 1/2 point to get point values
			dplyr::mutate(
				pts = round((estimate / 0.5))
			) |>
			# This effectively doubles the points so for scores where all the
			# numbers are even (no half points) we need to divide by two.
			dplyr::mutate(ind = all((pts %% 2) == 0)) |>
			dplyr::mutate(
				pts = dplyr::if_else(ind, pts / 2, pts)
			) |>
			dplyr::select(-ind)
	) |>
	# Above we get a data frame for each score. Bind them together into
	# one data frame with a variable indicating which score.
	dplyr::bind_rows(.id = "score") |>
	dplyr::select(-std.error, -statistic, -p.value) |>
	dplyr::rename(
		coefficient = estimate,
		lwr = conf.low,
		upr = conf.high
	)

# Save to disk
readr::write_rds(
	score_model_coefs,
	here::here("Results", "Tables", "Score-Model-Points-Tab-Pt.Rds")
)

# Get the scores for each patient -- we'll use the derivation data first
# so we can fit the risk models. Then we'll join everything separately for the
# validation data. This is sort of inefficient but it is probably the easiest
# way to prevent data leakage.
der_scores <-
	pcr_der |>
	dplyr::mutate(
		"cough:subjective_fever" = cough * subjective_fever,
		"cough:subjective_fever:acute_onset" = cough * subjective_fever * acute_onset,
		"cough:subjective_fever:myalgia" = cough * subjective_fever * myalgia,
		"chills_sweats:cough:subjective_fever" = chills_sweats * cough * subjective_fever
	) |>
	# Pivot data to long form where we have symptom | present/absent instead of
	# one column for each symptom
	tidyr::pivot_longer(
		cols = -c(unique_visit, diagnosis),
		names_to = "symptom",
		values_to = "present"
	) |>
	# Join the long data to the score model points. Right join so we only
	# keep the symptoms that are in the score models.
	dplyr::right_join(
		score_model_coefs,
		by = c("symptom" = "term")
	) |>
	# Calculate the scores for each person
	dplyr::group_by(unique_visit, score, diagnosis) |>
	dplyr::summarize(
		value = sum(pts * present),
		.groups = "drop"
	) |>
		# Coerce score to factor for pretty printing
		dplyr::mutate(score = factor(score))

## Fit the risk model ####
# we'll use a glm to get estimate risks for each value of the score. We do
# this only for the derivation set.
risk_models <-
	der_scores |>
	dplyr::mutate(
		diagnosis = as.numeric(diagnosis == "positive")
	) |>
	tidyr::nest(data = -score) |>
	dplyr::mutate(
		# Fit the GLM on the derivation set
		model = purrr::map(
			data,
			\(x) glm(
				formula = diagnosis ~ value,
				family = binomial(link = "logit"),
				data = x
			)
		),
		# Create new data that gets the unique values of each score, so we can
		# get the model predicted risk for each value.
		new_data = purrr::map(
			data,
			\(x) x |>
				dplyr::summarize(
					value = sort(unique(value))
				)
		),
		risk_preds = purrr::map2(
			model,
			new_data,
			# Get predictions on the new data, which is just one of every value
			# that the score could take on.
			\(mod, dat) broom::augment(
				mod,
				newdata = dat,
				type.predict = "link",
				se_fit = TRUE
			) |>
				# Construct the Wald-Type CI for the risk and then transform to
				# be on the probability (response) scale
				dplyr::mutate(
					lwr = .fitted + qnorm(0.025) * .se.fit,
					upr = .fitted + qnorm(0.975) * .se.fit,
					dplyr::across(
						c(.fitted, lwr, upr),
						plogis
					)
				) |>
				dplyr::select(value, fit = .fitted, lwr, upr)
		)
	)

# Lookup table of risks
risk_to_join <-
	risk_models |>
	dplyr::pull(risk_preds) |>
	rlang::set_names(risk_models$score) |>
	dplyr::bind_rows(.id = "score")

readr::write_rds(
	risk_to_join,
	file = here::here("Results", "Data", "Pt", "Score-Models",
										"Scores-to-Risks-Table.Rds")
)

# TODO move to figures
ggplot2::ggplot(risk_to_join) +
	ggplot2::aes(x = value, y = fit, ymin = lwr, ymax = upr) +
	ggplot2::geom_hline(yintercept = 0.5, color = "gray") +
	ggplot2::geom_hline(yintercept = 0.1, color = "gray") +
	ggplot2::geom_line() +
	ggplot2::geom_pointrange() +
	ggplot2::facet_wrap(ggplot2::vars(score)) +
	zlib::theme_ms()

## Get risk estimates for each individual ####

# I'll do this separately for pcr_der and pcr_val since there's only two of
# them and it was more convenient earlier to have them separate.
der_risks <-
	dplyr::left_join(
		der_scores,
		risk_to_join,
		by = c("score", "value")
	)

# We have to repeat a bunch of stuff for the validation set, but at least
# we are guaranteed to have no data leakage.
val_risks <-
	pcr_val |>
	dplyr::mutate(
		"cough:subjective_fever" = cough * subjective_fever,
		"cough:subjective_fever:acute_onset" = cough * subjective_fever * acute_onset,
		"cough:subjective_fever:myalgia" = cough * subjective_fever * myalgia,
		"chills_sweats:cough:subjective_fever" = chills_sweats * cough * subjective_fever
	) |>
	# Pivot data to long form where we have symptom | present/absent instead of
	# one column for each symptom
	tidyr::pivot_longer(
		cols = -c(unique_visit, diagnosis),
		names_to = "symptom",
		values_to = "present"
	) |>
	# Join the long data to the score model points. Right join so we only
	# keep the symptoms that are in the score models.
	dplyr::right_join(
		score_model_coefs,
		by = c("symptom" = "term")
	) |>
	# Calculate the scores for each person
	dplyr::group_by(unique_visit, score, diagnosis) |>
	dplyr::summarize(
		value = sum(pts * present),
		.groups = "drop"
	) |>
	# Coerce score to factor for pretty printing
	dplyr::mutate(score = factor(score)) |>
	dplyr::left_join(
		risk_to_join,
		by = c("score", "value")
	)

# Join together and create categorical outcome ####
risk_data <-
	dplyr::bind_rows(
		"derivation" = der_risks,
		"validation" = val_risks,
		.id = "sample"
	) |>
	dplyr::mutate(
		risk_group = dplyr::case_when(
			fit <= 0.10 ~ "low",
			fit <= 0.50 ~ "med",
			fit >  0.50 ~ "high"
		),
		risk_group = factor(
			risk_group,
			levels = c("low", "med", "high"),
			labels = c("Low", "Moderate", "High"),
			ordered = TRUE
		),
		sample = factor(
			sample,
			levels = c("derivation", "validation"),
			labels = c("Derivation group", "Validation group")
		),
		diagnosis = factor(
			diagnosis,
			levels = c("negative", "positive"),
			labels = c("Influenza -", "Influenza +")
		),
		model_group = "point score"
	) |>
	dplyr::rename(
		model = score,
		point_score = value,
		risk = fit,
		risk_lwr = lwr,
		risk_upr = upr
	)

readr::write_rds(
	risk_data,
	file = here::here("Results", "Data", "Pt", "Score-Models",
										"Score-Risk-Predictions-Raw.Rds")
)

risk_data_to_merge <-
	risk_data |>
	dplyr::select(
		sample,
		unique_visit,
		model,
		diagnosis,
		risk_pred = risk,
		risk_group,
		model_group
	)

readr::write_rds(
	risk_data_to_merge,
	file = here::here("Results", "Data", "Pt", "Score-Models",
										"Risk-Preds-Scores.Rds")
)

# END OF FILE ####
