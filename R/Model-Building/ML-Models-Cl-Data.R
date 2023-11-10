###
# ML Models ON CLINICIAN DATA
# Zane Billings
# 2022-11-17
# This script fits the tidymodels part of the models that we fit. 
# THESE MODELS ARE FIT TO THE CLINICIAN DATA. In the other ml-models script,
# they are fit to the patient data.
###

# Setup ####

# I use dark mode for my IDE so I set tidymodels to print light text.
# This can be set to FALSE if you use light mode IDE without changing any
# of the results.
options(tidymodels.dark = TRUE)

# Declare dependencies
box::use(
	dbarts,
	xgboost,
	earth,
	mda,
	klaR,
	discrim,
	kknn,
	ranger,
	kernlab,
	C50,
	lme4,
	dplyr,
	tidyr,
	readr,
	janitor,
	rsample,
	here
)

# Load all of tidymodels into the namespace because sometimes it gets
# mad if you don't.
library(tidymodels)

# Apparently the bonsai packge has to be loaded into the environment
# or else tidymodels doesn't know what to do. :eye roll emoji:
library(bonsai)

# Same for finetune package
library(finetune)

# Data processing ####

# Load the original data
dat_long <- readr::read_rds(here::here("Data/Clean-Data/Symptoms-Long.Rds"))

# Data cleaning ####
pcr_dat <-
	dat_long |>
	# Get only the PCR patient data
	dplyr::filter(
		method == "PCR", rater == "clinician"
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

# Load the data to get the IDs
dl <- readr::read_rds(here::here("Data", "Clean-Data", "model-data.Rds"))
pcr_der_pt <- dl[["derivation"]]
pcr_val_pt <- dl[["validation"]]

der_ids <- pcr_der_pt$unique_visit
val_ids <- pcr_val_pt$unique_visit

# Get the split
pcr_der <- pcr_dat |> dplyr::filter(unique_visit %in% der_ids)
pcr_val <- pcr_dat |> dplyr::filter(unique_visit %in% val_ids)

# Make resamples
# Set PRNG seed to ensure the splits are done the same way each time the code
# is run
set.seed(370)

# Create an object of data resamples. the resampling scheme is 10-fold CV
# repeated 100 times (Frank Harrell's recommendation), with sampling
# stratified by the diagnosis to ensure prevalence is roughly consistent
# in each fold.
pcr_rs <-
	pcr_der |>
	rsample::vfold_cv(
		v = 10,
		repeats = 100,
		strata = diagnosis
	)

# Tidymodels setup ####

## Recipe setup ####
# This specifies what data we should use for building models, and which
# variables are predictors/outcomes.

# Explicitly create the formula so nothing weird happens with using the dot
# formula operator
model_formula <-
	pcr_der |>
	dplyr::select(-diagnosis, -dx_num) |>
	colnames() |>
	reformulate(response = "diagnosis")

# Create the tidymodels recipe
my_rec <-
	recipes::recipe(model_formula, data = pcr_der) |>
	update_role(unique_visit, new_role = "ID")

## Model specifications ####
# These specify the different models that we will train, and which
# hyperparameters should be tuned.

model_specs <- list(
	### Bayesian additive regression trees (BART) ####
	"bart" =
		bart(
			mode = "classification",
			engine = "dbarts",
			trees = 1000,
			prior_terminal_node_coef = tune(),
			prior_terminal_node_expo = tune()
		),
	
	### Gradient-boosted tree with xgboost ####
	"xgboost" =
		boost_tree(
			mode = "classification",
			engine = "xgboost",
			mtry = tune(),
			trees = 1000,
			min_n = tune(),
			tree_depth = tune(),
			learn_rate = tune(),
			loss_reduction = tune(),
			sample_size = tune()
		),
	
	### CART decision tree (hyperparameter tuned) with rpart ####
	"cart" =
		decision_tree(
			mode = "classification",
			engine = "rpart",
			cost_complexity = tune(),
			tree_depth = tune(),
			min_n = tune()
		),
	
	### C5.0 decision tree (hyperparameter tuned) ####
	"C50" =
		decision_tree(
			mode = "classification",
			engine = "C5.0",
			min_n = tune()
		),
	
	### Logistic regression model (for comparison, no tuning) ####
	"glm" =
		logistic_reg(
			mode = "classification",
			engine = "glm"
		),
	
	### LASSO penalized logistic regression ####
	"lasso" =
		logistic_reg(
			mode = "classification",
			engine = "glmnet",
			penalty = tune(),
			mixture = 1
		),
	
	### Elastic net penalized logistic regression ####
	"en" =
		logistic_reg(
			mode = "classification",
			engine = "glmnet",
			penalty = tune(),
			mixture = tune()
		),
	
	### Naive Bayes classifier ####
	"nb" =
		naive_Bayes(
			mode = "classification",
			engine = "klaR",
			smoothness = tune(),
			Laplace = tune()
		),
	
	### k-nearest neighbors (kNN) ####
	"knn" =
		nearest_neighbor(
			mode = "classification",
			engine = "kknn",
			neighbors = tune()
		),
	
	### Random forest ####
	"rf" =
		rand_forest(
			mode = "classification",
			engine = "ranger",
			mtry = tune(),
			trees = tune(),
			min_n = tune()
		),
	
	### Support vector machine (SVM) with polynomial kernel ####
	"svmpol" =
		svm_poly(
			mode = "classification",
			engine = "kernlab",
			cost = tune(),
			degree = tune(),
			scale_factor = tune(),
			margin = tune()
		),
	
	### SVM with linear kernel ####
	"svmlin" =
		svm_linear(
			mode = "classification",
			engine = "kernlab",
			cost = tune(),
			margin = tune()
		),
	
	### SVM with RBF (radial basis function) kernel ####
	"svmrbf" =
		svm_rbf(
			mode = "classification",
			engine = "kernlab",
			cost = tune(),
			rbf_sigma = tune(),
			margin = tune()
		),
	
	### Conditional inference decision tree (hyperparameter tuned; ctree) ####
	"ctree" =
		decision_tree(
			mode = "classification",
			engine = "partykit",
			tree_depth = tune(),
			min_n = tune()
		)
)

## Bind the recipe together with each workflow in a workflowset ####
# A workflow combines a recipe with a model. A workflowset allows us to
# create several workflows simultaneously and iterate over them.
wfs <-
	workflow_set(
		# This is a list of recipes -- we only have one
		preproc = list(recipe = my_rec),
		# This is a list of models we want to fit
		models = model_specs,
		# We want to combine every recipe with every model
		cross = TRUE
	)

# Initialize the parallel processor ####
# if there are any issues with
# memory overflow, try reducing the amount of cores used as RAM is
# partitioned to all cores (therefore using less cores makes more RAM
# available to each core).
ncores <- dplyr::case_when(
	# If there are only one or two cores, run single core.
	parallel::detectCores() < 4 ~ 1,
	# If there are four cores, run on 3 of them
	parallel::detectCores() < 8 ~ 3,
	# If there are eight or more cores, run on all except 4 of them. This
	# allows for other stuff to keep going in the background. You can change
	# this if you want to use more cores, but n - 1 is the highest you should go.
	parallel::detectCores() >= 8 ~ parallel::detectCores() - 4
)

# Create the cluster -- using PSOCK should work on both Windows and Unix OS.
cl <- parallel::makePSOCKcluster(ncores)
# Register the cluster with the doParallel backend used by tidymodels.
doParallel::registerDoParallel(cl)

# Tune the models ####
# The tuning strategy will be a racing method over a grid search. Anything
# more complicated will be slower and is overkill for this project.
# A racing method eliminates poorly-performing parameter combinations before
# they are evaluated on every resample, so runs faster than grid search. In
# general, accuracy will not be lost by tossing out these hyperparameter sets
# as they tend not to improve when the entire grid search is run.
# The performance gain from racing methods improves when used in parallel, so
# you should expect to see faster-than-linear performance increases if you
# can use more cores for tuning.

# IF THE MODELS ARE ALREADY TUNED, DO NOT RETUNE BY DEFAULT. THIS IS
# COMPUTATIONALLY EXPENSIVE AND WILL TAKE AT LEAST AN HOUR, LIKELY MULTIPLE.
# If you *really* want or need to retune the models, you can manually
# delete the results or set the global variable RETUNE (initialized below)
# to TRUE.
# If the saved results do not already exist, then the tuning procedure
# will run regardless of how RETUNE is set.

retune <- FALSE
pth <- here::here("Results", "Data", "Cl", "ML-Models",
									"finalized-wfs.Rds")
chk <- file.exists(pth)
if (isFALSE(retune) & isTRUE(chk)) {
	
	# If we don't need to tune the models, load the saved results from disk.
	final_wf_set <- readRDS(pth)
	ids_to_map_over <- final_wf_set$wflow_id
	
} else {
	
	# Force the garbage collector to run -- this is almost certainly superfluous
	# but it makes me feel like I have more RAM available.
	invisible(gc())
	
	# This part actually does the model tuning using the racing method. The
	# grid will have 25 entries for each parameter (which gets crossed, resulting
	# in 25^p hyperparameter sets for each model where p is the number of tunable
	# hyperparameters of the model). Each of these is evaluated on the v * r
	# number of CV folds, which I set to be 10 * 100 = 1000. So this code can
	# take a bit to run, even in parallel with the racing method.
	set.seed(370)
	tune_res <-
		wfs |>
		workflow_map(
			"tune_race_anova",
			resamples = pcr_rs,
			grid = 25,
			metrics = metric_set(roc_auc),
			verbose = TRUE,
			seed = 370,
			control = control_race(
				verbose = TRUE,
				event_level = "second",
				allow_par = TRUE,
				parallel_over = "everything"
			)
		)
	
	# FOR EACH OF THE MODELS, select the best performing hyperparameter set (i.e.
	# the hyperparameter set with the best cross-validated AUROCC).
	best_models <-
		tune_res |>
		collect_metrics() |>
		dplyr::group_by(wflow_id) |>
		dplyr::slice_max(mean, n = 1, with_ties = FALSE) |>
		dplyr::ungroup()
	
	# Finalize all models -- this is more tidymodels lingo. For each of the model
	# workflows, set the values of the hyperparameters to be equal to the best
	# set that we selected based on AUROCC. This is complicated with the
	# workflowset so I wrote this function to handel it for one model at a time.
	finalize_wf_from_set <- function(wfs, id) {
		
		# Select the best model -- this does the same thing as the code before but
		# in "native" tidymodels language for one model.
		best_wf <-
			wfs |>
			extract_workflow_set_result(id) |>
			select_best()
		
		# Extract the un-finalized workflow, which has the tuning parameter
		# placeholder values.
		raw_wf <-
			wfs |>
			extract_workflow(id)
		
		# If it's a null model, don't try to finalize--this well mess it up.
		# Otherwise remove the tuning placeholders and specify the values using
		# the best hyperparameter set.
		if (grepl("null", id)) {
			finalized_wf <- raw_wf
		} else {
			finalized_wf <- finalize_workflow(raw_wf, best_wf)
		}
		
		# Return the finalized workflow.
		return(finalized_wf)
		
	}
	
	# Get the workflow IDs as a vector
	ids_to_map_over <-
		best_models |>
		dplyr::pull(wflow_id)
	
	# Iterate over the workflow IDs, finalizing each one
	final_wf_list <-
		purrr::map(ids_to_map_over, ~finalize_wf_from_set(tune_res, .x)) |>
		rlang::set_names(nm = ids_to_map_over)
	
	# Combine the results of the map call (a list of finalized workflows) into
	# one workflowset object, since there is currently no native way in
	# workflowsets to finalize an entire workflowset.
	final_wf_set <- do.call(workflowsets::as_workflow_set, final_wf_list)
	
	# Save finalized workflows to disk. This is the most space-efficient way
	# to save the hyperparameter info we need. If you need to save all of the
	# individual runs, you will need to add code for that, but the file will
	# be multiple GB in size.
	saveRDS(final_wf_set, pth)
}

# Getting model performance and predictions ####

## Resampled metrics ####
# Get the AUROCC on the set of resamples for each model -- this is technically
# redundant but it is much easier to refit the models than try to extract
# the metrics we need from the tuning results.
set.seed(370)
resampled_perf <-
	final_wf_set |>
	workflow_map(
		fn = "fit_resamples",
		resamples = pcr_rs,
		verbose = TRUE,
		seed = 370,
		control = control_resamples(
			verbose = TRUE,
			allow_par = TRUE,
			event_level = "second",
			parallel_over = "everything"
		)
	)

# This bit cleans up the metrics and gets only the AUROCC part we want.
resampled_metrics <-
	resampled_perf |>
	collect_metrics() |>
	dplyr::filter(.metric == "roc_auc")

# Function to fit the model to an analysis set of data, and then evaluate
# the performance on an assessment set of data.
# We will use this to ensure we do not have data leakage when we fit to the
# validation set, and it is also a convenient way to fit to the derivation
# set without tidymodels fighting too much.
get_fit_from_split <- function(wfs, id, split = NULL,
															 analysis = NULL, assessment = NULL,
															 S = 370) {
	# wfs is a workflowset with no tuning placeholders
	# id is a vector of workflow IDs
	# split is an rsample split object
	# analysis AND assessment must be BOTH specified if split is not specified.
	#  this function will create a split object using those two sets.
	# S is the random seed which I am always afraid not to set.
	
	# Construct the splitting object if split is NULL.
	if (is.null(split)) {
		set.seed(S)
		split <- rsample::make_splits(analysis, assessment = assessment)
	}
	
	# Get the finalized result out of the workflowset
	best_wf <-
		final_wf_set |>
		extract_workflow_set_result(id)
	
	# Extract the workflow from the object
	raw_wf <-
		wfs |>
		extract_workflow(id)
	
	# Fit the model to the analysis set of the split,
	# but get predictions and metrics from the assessment set.
	my_fit <-
		last_fit(
			raw_wf,
			split = split,
			metrics = metric_set(roc_auc),
			control = control_last_fit()
		)
	
	return(my_fit)
}

## Validation set performance ####
val_fits <- purrr::map(
	ids_to_map_over, ~get_fit_from_split(
		final_wf_set,
		.x,
		analysis = pcr_der,
		assessment = pcr_val
	)
)
names(val_fits) <- ids_to_map_over

val_metrics <-
	purrr::map(val_fits, ~.x[[".metrics"]][[1]]) |>
	dplyr::bind_rows(.id = "model") |>
	dplyr::filter(.metric == "roc_auc") |>
	dplyr::rename(
		wflow_id = model,
		mean = .estimate
	) |>
	dplyr::mutate(
		preproc = "recipe",
		n = 1,
		std_err = NA
	)

## Derivation set performance ####
der_split <- rsample:::make_splits(pcr_der, assessment = pcr_der)

der_fits <- purrr::map(
	ids_to_map_over, ~get_fit_from_split(final_wf_set, .x, split = der_split)
)
names(der_fits) <- ids_to_map_over

der_metrics <-
	purrr::map(der_fits, ~.x[[".metrics"]][[1]]) |>
	dplyr::bind_rows(.id = "model") |>
	dplyr::filter(.metric == "roc_auc") |>
	dplyr::rename(
		wflow_id = model,
		mean = .estimate
	) |>
	dplyr::mutate(
		preproc = "recipe",
		n = 1,
		std_err = NA
	)

# Turn off parallel processing ####
kill_doParallel <- function(cl = NULL) {
	doParallel::stopImplicitCluster()
	if (!is.null(cl)) {parallel::stopCluster(cl)}
	cl_env <- foreach:::.foreachGlobals
	rm(list = ls(name = cl_env), pos = cl_env)
}

kill_doParallel(cl)

raw_ml_preds <-
	dplyr::bind_rows(
		"derivation" = dplyr::bind_rows(der_fits, .id = "model"),
		"validation" = dplyr::bind_rows(val_fits, .id = "model"),
		.id = "sample"
	)

readr::write_rds(
	raw_ml_preds,
	file = here::here("Results", "Data", "Cl", "ML-Models",
										"ML-Risk-Predictions-Raw.Rds")
)

# Unnest the predictions and map back to the unique_visit in the raw data since
# tidymodels annoyingly only gives us the row number
unique_ids_to_join <-
	dplyr::bind_rows(
		"derivation" = pcr_der,
		"validation" = pcr_val,
		.id = "sample"
	) |>
	dplyr::group_by(sample) |>
	dplyr::transmute(
		unique_visit,
		.row = dplyr::row_number()
	)

# Clean up the raw predictions, join the row numbers back to the unique_visit
# identifiers, and format.
risk_data_to_merge <-
	raw_ml_preds |>
	tidyr::unnest(.predictions) |>
	dplyr::transmute(
		sample,
		model,
		risk_pred = .pred_positive,
		row_n = .row - 174,
		diagnosis
	) |>
	dplyr::left_join(
		unique_ids_to_join,
		by = c("sample", "row_n" = ".row")
	) |>
	dplyr::transmute(
		sample = factor(
			sample,
			levels = c("derivation", "validation"),
			labels = c("Derivation group", "Validation group")
		),
		unique_visit,
		model = gsub("recipe_", "", model),
		# model = dplyr::recode(
		# 	model,
		# 	"C50" = "C5.0 tree (tuned)",
		# 	"cart" = "CART tree (tuned)",
		# 	"ctree" = "Conditionl inference tree (tuned)",
		# 	"nb" = "Naive Bayes",
		# 	"knn" = "kNN",
		# 	"glm" = "Unpenalized logistic regression",
		# 	"lasso" = "LASSO logistic regression",
		# 	"en" = "Elastic net logistic regression",
		# 	"svmlin" = "SVM (linear kernel)",
		# 	"svmpol" = "SVM (polynomial kernel)",
		# 	"svmrbf" = "SVM (RBF kernel)",
		# 	"bart" = "BART",
		# 	"rf" = "Random forest",
		# 	"xgboost" = "Gradient-boosted tree (xgboost)"
		# ),
		diagnosis = factor(
			as.character(diagnosis),
			levels = c("negative", "positive"),
			labels = c("Influenza -", "Influenza +")
		),
		risk_pred,
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
		model_group = "machine learning"
	)

readr::write_rds(
	risk_data_to_merge,
	file = here::here("Results", "Data", "Cl", "ML-Models",
										"Risk-Preds-ML.Rds")
)

# Clean up AUC data for exporting ####
# Derivation and validation single-fit AUROCC
sample_auc <-
	raw_ml_preds |>
	tidyr::unnest(.metrics) |>
	dplyr::transmute(
		sample,
		model,
		auc = .estimate,
		se = NA_real_
	)

# Resampled AUROCC
resample_auc <-
	resampled_metrics |>
	dplyr::transmute(
		sample = "resample",
		model = wflow_id,
		auc = mean,
		se = std_err
	)

# Bind and save to disk
auc_data <-
	dplyr::bind_rows(sample_auc, resample_auc) |>
	dplyr::mutate(
		sample = factor(
			sample,
			levels = c("derivation", "resample", "validation"),
			labels = c(
				"Derivation group",
				"Cross-validated",
				"Validation group"
			)
		)
	)

readr::write_rds(
	auc_data,
	file = here::here("Results", "Data", "Cl", "ML-Models",
										"AUC-ML.Rds")
)

# END OF FILE ####
