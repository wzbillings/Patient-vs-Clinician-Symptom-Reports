# Previous rule validation
# Zane Billings
# 2022-08-21
# Copied from Annika's previous `table4recreation.Rmd`.
# Description: Using our data, recreate a table similar to Table 4 in the
#  Ebell 2021 systematic review. This shows how the CPRs perform on
#  our data.

# Setup ####
# Creating a function to calculate the likelihood ratio with 3 strata
# low, moderate, and high

source(here::here("R", "Common-Functions", "Stratum-Specific-Stats.R"))

# Require dependencies
box::use(
	readr,
	tidyr,
	dplyr,
	purrr,
	yardstick
)

# Data importing and processing ####
score_dat <- readRDS(here::here("Results", "Data", "Score-Data.Rds"))

score_stats <-
	score_dat |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("res"),
		names_to = "method",
		values_to = "dx",
		names_prefix = "res_"
	) |>
	tidyr::drop_na(dx) |>
	dplyr::select(unique_visit, method, dx, tidyselect::ends_with("Pred")) |>
	tidyr::pivot_longer(starts_with("pt") | starts_with("cl")) |>
	dplyr::mutate(
		name = stringr::str_remove(name, "_Pred"),
		value = factor(value, levels = c("low", "med", "high"), ordered = TRUE)
	) |>
	tidyr::separate(name, c("rater", "score")) |>
	tidyr::nest(data = -c(rater, score, method)) |>
	dplyr::mutate(
		stats = purrr::map(data, ~stratum_specific_stats(.x[[2]], .x[[3]]))
	) |>
	tidyr::unnest(stats)

score_auc <-
	score_dat |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("res"),
		names_to = "method",
		values_to = "dx",
		names_prefix = "res_"
	) |>
	tidyr::drop_na(dx) |>
	dplyr::select(unique_visit, method, dx, tidyselect::ends_with("Score")) |>
	tidyr::pivot_longer(starts_with("pt") | starts_with("cl")) |>
	dplyr::mutate(
		name = stringr::str_remove(name, "_Score"),
		dx = factor(dx, levels = c("FALSE", "TRUE"), labels = c("-", "+")),
	) |>
	tidyr::separate(name, c("rater", "score")) |>
	tidyr::nest(data = -c(rater, score, method)) |>
	dplyr::mutate(
		auc = purrr::map_dbl(
			.x = data,
			.f = ~yardstick::roc_auc(
				data = .x,
				truth = dx,
				estimate = value,
				event_level = "second"
			) |>
				dplyr::pull(.estimate)
		)
	)

score_stat_df <-
	dplyr::left_join(
		x = score_stats |> dplyr::select(!data),
		y = score_auc   |> dplyr::select(!data),
		by = c("rater", "score", "method")
	) |>
	dplyr::mutate(
		dplyr::across(
			.cols = c(ratio, percflu),
			~ifelse(is.nan(.x), NA_real_, .x)
		),
		percgroup = ifelse(percflu == 0, NA, percgroup),
		score = factor(
			score,
			levels = c("CF", "CFA", "CFM", "Ebell3", "Tree1"),
			labels = c("CF", "CFA", "CFM", "Score", "Tree")
		)
	) |>
	dplyr::mutate(
		group = gsub("moderate", "mod", group),
		group = stringr::str_to_sentence(group),
		group = factor(group, levels = c("Low", "Mod", "High"),
									 ordered = TRUE),
		rater = factor(rater, levels = c("pt", "cl"),
									 labels = c("Patient", "Clinician"), ordered = TRUE),
		lr_col = paste0(group, ": ", sprintf("%.1f", percflu * 100), "%, LR ",
										sprintf("%.2f", ratio)),
		lr_col = ifelse(!is.na(ratio), lr_col, "Mod: NA"),
		pp_col = paste0(group, ": ", sprintf("%.1f", percgroup * 100), "%"),
		pp_col = ifelse(!is.na(ratio), pp_col, "Mod: NA")
	) |>
	dplyr::arrange(method, score, rater, group)

# Export to results file ####
readr::write_rds(
	score_stat_df,
	here::here("Results", "CPR-Assessment-Data.Rds")
)

# END OF FILE ####
