###
# Threshold sensitivity analysis
# Zane Billings
# 2023-01-20
# In this script, I do a sensitivity analysis for the risk groups by
# trying a few different training/testing threshold combinations.
# I'll also make a plot of the risk histograms for the supplement.
###

# Setup ####

# Load the risk predictions data
preds <- readr::read_rds(here::here("Results", "Data", "Model-Predictions.rds"))

# Load function for stratum-specific statistics
source(here::here("R", "Common-Functions", "Stratum-Specific-Stats.R"))

# Require dependencies
box::use(
	ggplot2,
	scales,
	dplyr,
	readr,
	zlib,
	tidyr
)

## ggplot 2 theme setting ####
plot_palette <- c(
	scales::viridis_pal(begin = 0, end = 0.8, option = "plasma")(2),
	"red"
)

ggplot2::theme_set(ggplot2::theme_classic(
	base_size = 22,
))

ggplot2::theme_update(
	legend.position = "bottom",
	legend.justification = "center",
	text = ggplot2::element_text(color = "black"),
	axis.text = ggplot2::element_text(color = "black")
)

# Risk histograms ####

risk_histogram <-
	preds |>
	dplyr::filter(model %in% c("LASSO", "Conditional inference", "nb")) |>
	dplyr::mutate(
		model = dplyr::recode(
			model,
			"LASSO" = "LASSO score",
			"LASSO variables (unweighted)" = "LASSO heuristic",
			"A priori symptoms" = "A priori symptom score",
			"CF (unweighted)" = "Cough/fever heuristic",
			"CF (weighted)" = "Cough/fever symptom score",
			"CFA (unweighted)" = "Cough/fever/acute onset heuristic",
			"CFA (weighted)" = "Cough/fever/acute onset symptom score",
			"CFM (unweighted)" = "Cough/fever/myalgia heuristic",
			"CFM (weighted)" = "Cough/fever/myalgia symptom score",
			"Ebell flu score symptoms" = "Re-fit FluScore model (Ebell 2012)",
			"CART" = "CART (manual)",
			"FFT" = "FFT",
			"C5.0" = "C5.0 tree (manual)",
			"Conditional inference" = "Conditional inference tree (manual)",
			"bart" = "Bayesian Additive Regression Trees (BART)",
			"C50" = "C5.0 tree (tuned)",
			"cart" = "CART (tuned)",
			"ctree" = "Conditionl inference tree (tuned)",
			"en" = "Elastic net logistic regression",
			"glm" = "Unpenalized logistic regression",
			"knn" = "k-Nearest Neighbors classifier",
			"lasso" = "LASSO logistic regression",
			"nb" = "Naive Bayes classifier",
			"rf" = "Random forest",
			"svmlin" = "SVM (linear kernel)",
			"svmpol" = "SVM (polynomial kernel)",
			"svmrbf" = "SVM (RBF kernel)",
			"xgboost" = "Gradient-boosted tree"
		) |> factor()
	) |>
	ggplot2::ggplot() +
	ggplot2::aes(
		x = risk_pred,
		fill = rater
	) +
	ggplot2::geom_histogram(
		ggplot2::aes(y = ..density..),
		position = "identity",
		boundary = 0.05,
		binwidth = 0.05,
		alpha = 0.5,
		color = "black"
	) +
	ggplot2::facet_grid(
		rows = ggplot2::vars(model),
		cols = ggplot2::vars(sample),
		scales = "free_y",
		switch = "y"
	) +
	ggplot2::labs(
		x = "predicted risk",
		y = "Density"
	) +
	ggplot2::scale_x_continuous(
		expand = c(0, 0),
		limits = c(-0.01, 1.01),
		minor_breaks = seq(0, 1, 0.05),
		breaks = seq(0, 1, 0.25),
		labels = c("0", "", "0.5", "", "1")
	) +
	ggplot2::scale_y_continuous(
		position = "right"
	) +
	ggplot2::scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
	zlib::theme_ms() +
	ggplot2::theme(
		strip.text = ggplot2::element_text(
			margin = margin(b = 2),
			size = 16
		),
		strip.text.y.left = ggplot2::element_text(
			size = 14,
			angle = 0,
			margin = margin(r = 5),
			hjust = 0
		),
		plot.background = ggplot2::element_rect(
			color = "white"
		)
	)

ggplot2::ggsave(
	here::here("Results", "Figures", "risk-histograms.tiff"),
	width = 12,
	height = 6
) |>
	suppressWarnings()

# 25/60 threshold ####
# Recommended by Cai et al 2022 in the context of home rapid testing
res_2560 <-
	preds |>
	dplyr::select(-risk_group) |>
	dplyr::mutate(
		risk_group = factor(
			dplyr::case_when(
				risk_pred <= 0.25 ~ "Low",
				risk_pred <= 0.60 ~ "Moderate",
				risk_pred >  0.60 ~ "High",
				TRUE ~ NA_character_
			),
			levels = c("Low", "Moderate", "High"),
			ordered = TRUE
		),
		.after = risk_pred
	)

res_2560_tab <-
	res_2560 |>
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

# 30/70 threshold ####
# A more extreme threshold we picked based on the risk histograms
res_3070 <-
	preds |>
	dplyr::select(-risk_group) |>
	dplyr::mutate(
		risk_group = factor(
			dplyr::case_when(
				risk_pred <= 0.30 ~ "Low",
				risk_pred <= 0.70 ~ "Moderate",
				risk_pred >  0.70 ~ "High",
				TRUE ~ NA_character_
			),
			levels = c("Low", "Moderate", "High"),
			ordered = TRUE
		),
		.after = risk_pred
	)

res_3070_tab <-
	res_3070 |>
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

# Blahj ####
risk_groups <-
	dplyr::bind_rows(
		"10/50" = preds,
		"25/60" = res_2560,
		"30/70" = res_3070,
		.id = "thresholds"
	)

risk_groups_tab_data <-
	risk_groups |>
	dplyr::mutate(
		diag2 = (diagnosis == "Influenza +")
	) |>
	dplyr::filter(model %in% c("LASSO", "Conditional inference", "nb")) |>
	dplyr::mutate(
		model = dplyr::recode(
			model,
			"LASSO" = "LASSO score",
			"LASSO variables (unweighted)" = "LASSO heuristic",
			"A priori symptoms" = "A priori symptom score",
			"CF (unweighted)" = "Cough/fever heuristic",
			"CF (weighted)" = "Cough/fever symptom score",
			"CFA (unweighted)" = "Cough/fever/acute onset heuristic",
			"CFA (weighted)" = "Cough/fever/acute onset symptom score",
			"CFM (unweighted)" = "Cough/fever/myalgia heuristic",
			"CFM (weighted)" = "Cough/fever/myalgia symptom score",
			"Ebell flu score symptoms" = "Re-fit FluScore model (Ebell 2012)",
			"CART" = "CART (manual)",
			"FFT" = "FFT",
			"C5.0" = "C5.0 tree (manual)",
			"Conditional inference" = "Conditional inference tree (manual)",
			"bart" = "Bayesian Additive Regression Trees (BART)",
			"C50" = "C5.0 tree (tuned)",
			"cart" = "CART (tuned)",
			"ctree" = "Conditionl inference tree (tuned)",
			"en" = "Elastic net logistic regression",
			"glm" = "Unpenalized logistic regression",
			"knn" = "k-Nearest Neighbors classifier",
			"lasso" = "LASSO logistic regression",
			"nb" = "Naive Bayes classifier",
			"rf" = "Random forest",
			"svmlin" = "SVM (linear kernel)",
			"svmpol" = "SVM (polynomial kernel)",
			"svmrbf" = "SVM (RBF kernel)",
			"xgboost" = "Gradient-boosted tree"
		) |> factor()
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
	risk_groups_tab_data,
	here::here("Results", "Data", "risk-group-thresholds-data.Rds")
)

# END OF FILE ####
