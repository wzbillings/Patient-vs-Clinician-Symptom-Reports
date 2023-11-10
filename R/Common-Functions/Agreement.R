###
# Agreement metrics
# Zane Billings, Annika Cleven
# 2022-07-01
#In this script the functions for agreement (percentage agreement, standardized
#mean difference, cohen's kappa, pahbek,Gwets AC 1,Krippendorf's Alpha) are
#created.
###

## Percentage Agreement
## Percentage Agreement is the accuracy turned into a percentage.


pa_vec <- function(truth, estimate, ...) {
	compare <- truth == estimate
	return(mean(compare) * 100)
}

## Standardized Mean Difference
## The calculated distance between two groups means in the terms of 
## one or more variables 


smd_vec <- function(truth, estimate, ...) {
	x <- as.logical(truth)
	y <- as.logical(estimate)
	
	diff <- abs(mean(x) - mean(y))
	
	s_p <- sqrt((sd(x)^2  + sd(y)^2) / 2)
	
	if (length(truth) <= 50) {
		n <- length(truth)
		cf <- ((n - 3) / (n - 2.25)) * sqrt((n-2) / n)
		out <- (diff / s_p) * cf
	} else {
		out <- diff / s_p
	}
	
	return(out)
}

## Cohens Kappa
## Cohen's Kappa statistic measures interrater reliability and takes chance
## into account. A score of 0  = agreement equivalent to chance,
## 1 = perfect alignment. This statistic is highly dependent on 
## prevalence.

kappa_vec <- function(truth, estimate, method = "cohen", ...) {
	data <- table(truth, estimate)
	est_obj <- epiR::epi.kappa(data, method = method, ...)
	return(est_obj$kappa$est)
}

## Prevalence and bias adjusted kappa (PABAK)
## In response to the Cohen's Kappa's dependance on prevalence, this IRR 
## statistic was calculated. PABAK ignores the variation of prevalence across 
## condition and absence of bias. 

pabak_vec <- function(truth, estimate, method = "cohen", ...) {
	data <- table(truth, estimate)
	est_obj <- epiR::epi.kappa(data, method = method, ...)
	return(est_obj$pabak$est)
}

## Gwets AC 1
## It was created to overcome the issues that kappa has with being sensitive
## to trait prevalence and rater's classification probabilities. Another 
## reason it is more robust is that it does not depend on independence 
## between raters like the other kappa related statistics.


ac1_vec <- function(truth, estimate, ...) {
	data <- table(truth, estimate)
	est_obj <- irrCAC::gwet.ac1.table(data)
	return(est_obj$coeff.val)
}

## Krippendorfs Alpha
## Krippendorff's Alpha is an alternative to Cohen's Kappa.  It is different
## than other raters because it calculated the disagreement, rather than
## agreement. It is the observed disagreement corrected for disagreement
## by chance.  1 is perfect agreement, 0 no agreement beyond chance,
## negative vales indicate inverse agreement

alpha_vec <- function(truth, estimate, ...) {
	data <- table(truth, estimate)
	est_obj <- irrCAC::krippen2.table(data)
	return(est_obj$coeff.val)
}


#DATA FRAME CALCULATIONS

## Percentage agreement

# pa <- function(data, i, ...) {
# 	d <- data[i, ]
# 	out <- yardstick::accuracy_vec(d$patient, d$clinician)
# 	return(out)
# }
# 
# ## Standardized Mean Difference
# 
# smd <- function(data, i, ...) {
# 	d <- data[i, ]
# 	out <- smd_vec(d$patient, d$clinician)
# 	return(out)
# }
# 
# ## Cohen's Kappa
# 
# kappa <- function(data, i, ...) {
# 	d <- data[i, ]
# 	out <- kappa_vec(d$patient, d$clincian)
# 	return(out)
# }
# 
# ## Pabak
# 
# pabak <- function(data, i, ...) {
# 	d <- data[i, ]
# 	out <- pabak_vec(d$patient, d$clinician)
# 	return(out)
# }
# 
# ## Gwets AC 1
# 
# ac1 <- function(data, i, ...) {
# 	d <- data[i, ]
# 	out <- ac1_vec(d$patient, d$clinician)
# 	return(out)
# }
# 
# 
# ## Krippendorfs Alpha
# 
# alpha <- function(data, i, ...) {
# 	d <- data[i, ]
# 	out <- alpha_vec(d$patient, d$clinician)
# 	return(out)
# }

# Function to calculate all statistics on same bootstrap sample ####

compute_irr_stats_from_subset <- function(data, i, col1, col2){
	# Get current resample
	d <- data[i,]
	
	# Get the columns to make life easier
	# Using `[[` notation means it works the same regardless of if data is a
	# tibble or a regular data frame
	col1 <- d[[col1]]
	col2 <- d[[col2]]
	
	# Get the cross-table for later on
	tbl <- table(col2, col1)
	
	# Percent Agreement Calculations
	pa <- mean(col1 == col2)

	# Cohen's Kappa and PABAK Calculation
	est_obj <- epiR::epi.kappa(tbl, method = "cohen")
	cohen <- est_obj$kappa$est
	pabak <- est_obj$pabak$est
	
	#Gwet's AC1 Calculation
	est_obj <- irrCAC::gwet.ac1.table(tbl)
	gwet <- est_obj$coeff.val
	
	#Krippendorf's Alpha
	est_obj <- irrCAC::krippen2.table(tbl)
	krippen <- est_obj$coeff.val
	
	# AUC calculation
	auc <- yardstick::roc_auc_vec(
		# For this fcn, need to convert truth to factor
		truth = as.factor(col1),
		# And estimate to numeric
		estimate = as.numeric(col2),
		event_level = "second"
	)
	
	return(c(pa, cohen, pabak, gwet, krippen, auc))
}

# Functions for computing the statistics
# This function computes the bootstrap CI from a boot.out object and tidies
# it into a named numeric vector which is coercable to data.frame/tibble
tidy_boot_ci <- function(boot.out, ...) {
	res <-
		boot.out |>
		boot.ci(...)
	out <- c(
		"estimate" = res$t0,
		"lower" = res$percent[4],
		"upper" = res$percent[5]
	)
	return(out)
}




