# Score calculation interrater agreement
# Zane Billings
# 2022-08-19
# Calculates agreement statistics between the score results from the patients
# and from the clinicians.
# Kappas and related statistics for the heuristic rules
# Spearman and ICC for the score/tree rules

# Setup ####

# Load necessary agreement functions
source(here::here("R", "Common-Functions", "Agreement.R"))

# If you don't load the boot namespace, sometimes it acts weird.
library(boot)

# Import data ####
dat_orig <- readRDS(here::here("Results", "Data", "Score-Data.Rds"))

score_dat <-
	dat_orig |>
	dplyr::select(
		unique_visit,
		tidyselect::starts_with("res"),
		tidyselect::ends_with("Score")
	) |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("res"),
		names_to = "method",
		names_prefix = "res_",
		values_to = "diagnosis"
	) |>
	tidyr::drop_na(diagnosis) |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("pt") | tidyselect::starts_with("cl"),
		values_to = "score"
	) |>
	tidyr::separate(
		col = name,
		into = c("rater", "CPR", NA),
		sep = "_",
		extra = "merge"
	) |>
	dplyr::filter(CPR != "Tree2") |>
	dplyr::mutate(
		CPR = factor(
			CPR,
			levels = c("CF", "CFA", "CFM", "Ebell3", "Tree1"),
			labels = c("CF", "CFA", "CFM", "Score", "Tree")
		)
	) |>
	tidyr::pivot_wider(
		names_from = rater,
		values_from = score
	)

saveRDS(score_dat, here::here("Results/Data/long_score_dat.Rds"))

# Kappa-type statistics for heuristic rules
heur_dat <-
	score_dat |>
	dplyr::filter(CPR %in% c("CF", "CFA", "CFM")) |>
	dplyr::mutate(CPR = forcats::fct_drop(CPR))

# This function computes bootstrap CI's for all 5 IRR statistics for
# one data frame
compute_irr_boot_estimates <- function(dat, B = 10000, S = 370) {
	# Hopefully setting the seed means that the same indices are generated each
	# time. I'm actually not sure and it doesn't matter that much but it's
	# easy to put this here and hope it works.
	set.seed(S)
	
	boot.out <-
		boot(
			data = dat,
			R = B,
			statistic = compute_irr_stats_from_subset,
			col1 = "cl",
			col2 = "pt",
			parallel = "multicore",
			ncpus = parallel::detectCores() - 2
		)
	
	res <-
		tibble::tibble(
			statistic = c("percent agreement",
										"Cohen's kappa",
										"PABAK",
										"Gwet's AC1",
										"Krippendorff's alpha"),
			purrr::map_dfr(
				.x = 1:5,
				.f = ~tidy_boot_ci(boot.out, index = .x, type = "perc", R = B)
			)
		)
	
	return(res)
}

set.seed(370)
heur_nested <-
	heur_dat |>
	tidyr::nest(data = -c(method, CPR)) |>
	dplyr::mutate(
		stats = purrr::map(data, compute_irr_boot_estimates)
	)

heur_stats <-
	heur_nested |>
	dplyr::select(-data) |>
	tidyr::unnest(stats) |>
	dplyr::mutate(
		agreement = dplyr::case_when(
			estimate <= 0.00 ~ "Negative",
			estimate <= 0.20 ~ "None",
			estimate <= 0.39 ~ "Minimal",
			estimate <= 0.59 ~ "Weak",
			estimate <= 0.79 ~ "Moderate",
			estimate <= 0.90 ~ "High",
			estimate <= 1.00 ~ "Almost Perfect"
		),
		# Agreement probably doesn't apply to Krippendorf's alpha
		# so set those to NA
		agreement = dplyr::if_else(statistic == "Krippendorf's alpha",
															 NA_character_, agreement),
		agreement = factor(agreement,
											 levels = c("Negative", "None", "Minimal", "Weak",
											 					 "Moderate", "High", "Almost Perfect"),
											 ordered = TRUE) |> forcats::fct_drop()
	) |>
	# Alphabetize symptoms because it makes me happy
	dplyr::arrange(CPR)

saveRDS(heur_stats, here::here("Results/Data/heurisics_agreement.Rds"))

# Correlations for numeric scores
cpr_dat <-
	score_dat |>
	dplyr::filter(!(CPR %in% c("CF", "CFA", "CFM"))) |>
	dplyr::mutate(CPR = forcats::fct_drop(CPR))

compute_corrs_from_subset <- function(data, i, col1, col2){
	# Get current resample
	d <- data[i,]
	
	# Get the columns to make life easier
	# Using `[[` notation means it works the same regardless of if data is a
	# tibble or a regular data frame
	col1 <- d[[col1]]
	col2 <- d[[col2]]
	
	# Spearman rank correlation
	rho <- cor(x = col1, y = col2, method = "spearman")
	
	# ICC
	est <- irr::icc(
		ratings = data.frame(col1, col2) |> as.matrix(),
		model = "twoway",
		type = "agreement",
		unit = "single"
	)
	icc <- est$value
	
	return(c(rho, icc))
}

compute_corr_boot_estimates <- function(dat, B = 10000, S = 370) {
	# Hopefully setting the seed means that the same indices are generated each
	# time. I'm actually not sure and it doesn't matter that much but it's
	# easy to put this here and hope it works.
	set.seed(S)
	
	boot.out <-
		boot(
			data = dat,
			R = B,
			statistic = compute_corrs_from_subset,
			col1 = "cl",
			col2 = "pt",
			parallel = "multicore",
			ncpus = parallel::detectCores() - 2
		)
	
	res <-
		tibble::tibble(
			statistic = c("rho", "ICC"),
			purrr::map_dfr(
				.x = 1:2,
				.f = ~tidy_boot_ci(boot.out, index = .x, type = "perc", R = B)
			)
		)
	
	return(res)
}

set.seed(370)
cpr_nested <-
	cpr_dat |>
	tidyr::nest(data = -c(method, CPR)) |>
	dplyr::mutate(
		stats = purrr::map(data, compute_corr_boot_estimates)
	)

cpr_stats <-
	cpr_nested |>
	tidyr::unnest(stats) |>
	dplyr::select(-data)

saveRDS(cpr_stats, here::here("Results/Data/num_cpr_agreement.Rds"))

