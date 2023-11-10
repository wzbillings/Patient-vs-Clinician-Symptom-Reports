###
# Data processing for model building
# Zane Billings
# 2022-09-29
# In this script, I'll do all the extra data processing that's necessary
# for fitting the score, tree, and ML models. This will ensure that
# we don't have any data leakage across the models that can't be fit
# with tidymodels.
# Copied from the Model-Building.qmd document.
###

# SETUP ####

box::use(
	dplyr,
	tidyr,
	readr,
	janitor,
	rsample,
	here
)

# Load the original data
dat_long <- readr::read_rds(here::here("Data/Clean-Data/Symptoms-Long.Rds"))

# Data cleaning ####
pcr_dat <-
	dat_long |>
	# Get only the PCR patient data
	dplyr::filter(
		method == "PCR", rater == "patient"
	) |>
	# Select only the variables that will go in the models
	dplyr::select(unique_visit, diagnosis, symptom, present) |>
	# Pivot so each symptom is its own variable
	tidyr::pivot_wider(
		names_from = symptom,
		values_from = present
	) |>
	# Remove spaces from the names
	janitor::clean_names() |>
	# Creating new variables for modeling
	dplyr::mutate(
		# Coerce lgl to num or tidymodels will cry and complain
		dplyr::across(tidyselect:::where(is.logical), as.numeric),
		# Numeric version of outcome variable
		dx_num = as.numeric(diagnosis == "positive")
	)

# Derivation/validation split ####
# (AKA train/test) -- 70% derivation
set.seed(370)
pcr_split <-
	pcr_dat |>
	rsample::initial_split(
		prop = 0.7,
		strata = diagnosis
	)

data_list <-
	list(
		"split"      = pcr_split,
		"derivation" = rsample::training(pcr_split),
		"validation" = rsample::testing(pcr_split)
	)

# Save data files to disk ####
data_list |>
	readr::write_rds(
		here::here("Data", "Clean-Data", "model-data.Rds")
	)

# END OF FILE ####