###
# Interrater reliability metric calculations
# Zane Billings
# 2021-08-31
# In this script, I'll calculate several metrics of interrater reliability for
# each symptom between clinicians and patients. For each metric, I'll compute
# a 95% bootstrap CI.
###

# Load the entire boot library since I think it doesn't always work right
# if you don't do this
library(boot)

# Wrapper functions for specific statistics ###################################

source(here::here("R", "Common-Functions", "Agreement.R"))

# Data Setup ##################################################################

# Load data
symp <- readRDS(here::here("Data", "Clean-Data", "Symptoms.Rds"))

# Pivot symptom data to longest format
symp_df <-
	symp |>
	dplyr::select(
		unique_visit,
		starts_with("res"),
		starts_with("cl"),
		starts_with("pt")
	) |>
	tidyr::pivot_longer(
		cols = starts_with("res"),
		names_to = "method",
		values_to = "diagnosis",
		names_prefix = "res_"
	) |>
	tidyr::drop_na(diagnosis) |>
	tidyr::pivot_longer(
		cols = starts_with("cl") | starts_with("pt"),
		names_to = "Symptom",
		values_to = "present"
	) |>
	tidyr::separate(
		col = Symptom,
		into = c("rater", "symptom"),
		sep = "_",
		extra = "merge"
	) |>
	dplyr::filter(symptom != "duration") |>
	dplyr::mutate(
		rater = factor(rater,
									 levels = c("cl", "pt"),
									 labels = c("clinician", "patient"))
	) |>
	tidyr::pivot_wider(
		names_from = rater,
		values_from = present
	) |>
	dplyr::mutate(
		across(c(clinician, patient), as.factor)
	)

# Construct bootstrap CIs ####################################################

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
			col1 = "clinician",
			col2 = "patient",
			parallel = "multicore",
			ncpus = max(parallel::detectCores() %/% 2, 1)
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

# Nest the data by symptom and get estimates for each symptom
# Set the seed here also because I am paranoid
set.seed(370)
symp_nested <-
	symp_df |>
	tidyr::nest(data = -c(symptom, method)) |>
	dplyr::mutate(
		stats = purrr::map(data, compute_irr_boot_estimates, B = 10000)
	)

symp_stats <-
	symp_nested |>
	dplyr::select(-data) |>
	tidyr::unnest(stats)

# Formatting so figure will look nice
symp_formatted <-
	symp_stats |>
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
											 ordered = TRUE),
		symptom = symptom |>
			stringr::str_replace(pattern = "Pn", replacement = "Pain") |>
			stringr::str_replace(pattern = "CoughYN", replacement = "Cough") |>
			stringr::str_replace(pattern = "ChillsSweats",
													 replacement = "ChillsOrSweats") |>
			stringr::str_replace(pattern = "Acute", replacement = "Acute Onset") |>
			snakecase::to_title_case() |>
			as.factor()
	) |>
	# Alphabetize symptoms because it makes me happy
	dplyr::arrange(symptom)

saveRDS(symp_formatted, here::here("Results", "Data",
																	 "symptoms_irr_df.Rds"))

# END OF FILE####
