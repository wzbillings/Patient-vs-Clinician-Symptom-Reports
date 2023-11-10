###
# Diagnostic agreement metric calculations
# Zane Billings
# 2022-08-15
# In this script, I'll calculate several metrics of interrater reliability for
# comparing clinician diagnosis accuracy with lab tests.
###

# Declare dependencies
box::use(
	boot[...],
	readr,
	dplyr,
	tidyr,
	tidyselect,
	tibble,
	purrr,
)

# Wrapper functions for specific statistics ###################################

source(here::here("R", "Common-Functions", "Agreement.R"))

# Data Setup ##################################################################

# Load data
symp <- readr::read_rds(here::here("Data", "Clean-Data", "Symptoms.Rds"))

# Pivot symptom data to longest format
res_df <-
	symp |>
	dplyr::select(
		unique_visit,
		tidyselect::starts_with("res")
	) |>
	dplyr::rename(clinician = res_clinician) |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("res"),
		names_to = "method",
		values_to = "lab_dx",
		names_prefix = "res_"
	) |>
	dplyr::mutate(
		dplyr::across(c("clinician", "lab_dx"), as.factor)
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
			col2 = "lab_dx",
			parallel = "multicore",
			ncpus = 16L
		)
	
	res <-
		tibble::tibble(
			statistic = c("percent agreement",
										"Cohen's kappa",
										"PABAK",
										"Gwet's AC1",
										"Krippendorff's alpha",
										"AUC"),
			purrr::map_dfr(
				.x = 1:6,
				.f = ~tidy_boot_ci(boot.out, index = .x, type = "perc", R = B)
			)
		)
	
	return(res)
}

# Nest the data by symptom and get estimates for each symptom
# Set the seed here also because I am paranoid
set.seed(370)
res_nested <-
	res_df |>
	tidyr::nest(data = -method) |>
	dplyr::mutate(
		# Because of the way the data were stored, we need to drop NAs
		data = purrr::map(data, tidyr::drop_na),
		# Now compute the statistics
		stats = purrr::map(data, compute_irr_boot_estimates)
	)

res_stats <-
	res_nested |>
	dplyr::select(-data) |>
	tidyr::unnest(stats)

# Formatting so figure will look nice
res_formatted <-
	res_stats |>
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
		# Agreement probably doesn't apply to non-Kappa-based stats
		# so set those to NA
		agreement = dplyr::if_else(
			!statistic %in% c("Cohen's kappa", "PABAK", "Gwet's AC1"),
			NA_character_, agreement),
		# Coerce to ordered factor
		agreement = factor(agreement,
											 levels = c("Negative", "None", "Minimal", "Weak",
											 					 "Moderate", "High", "Almost Perfect"),
											 ordered = TRUE) |> forcats::fct_drop(),
		method = method |>
			stringr::str_replace(pattern = "any_test", replacement = "either")
	) |>
	# Alphabetize symptoms because it makes me happy
	dplyr::arrange(method)

readr::write_rds(
	res_formatted,
	here::here("Results", "Data", "diagnosis-agreement-df.Rds")
)

# END OF FILE ####
