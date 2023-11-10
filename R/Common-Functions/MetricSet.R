##############################################################################
# Metric Set Creation
# Annika Cleven
# 2022-06-30
##############################################################################

#Create functions for the metric set 
#Functions created: positive likelihood ratio, negative likelihood ratio,
#diagnostic odds ratio, prevalence, markedness

#The metric set (mset) includes roc_auc, sens, spec, ppv, 
#npv, accuracy, accuracy2, posLR,negLR, DOR, bal_accuracy, mcc,prevalence, 
#detection_prevalence, pr_auc, mn_log_loss, markedness, j_index

# Copied from https://yardstick.tidymodels.org/articles/custom-metrics.html
event_col <- function(xtab, event_level) {
	if (identical(event_level, "first")) {
		colnames(xtab)[[1]]
	} else {
		colnames(xtab)[[2]]
	}
}

##POSITIVE LIKELIHOOD RATIO
posLR_vec <- function(truth, 
											estimate, 
											estimator = NULL, 
											na_rm = TRUE, 
											event_level = "first",
											...) {
	estimator <- finalize_estimator(truth, estimator)
	
	posLR_impl <- function(truth, estimate) {
		# Create 
		xtab <- table(estimate, truth)
		col <- event_col(xtab, event_level)
		col2 <- setdiff(colnames(xtab), col)
		
		tp <- xtab[col, col]
		fp <- xtab[col, col2]
		fn <- xtab[col2, col]
		tn <- xtab[col2, col2]
		
		(tp/(tp + fn))/(1-(tn/(tn+fp)))
	}
	
	metric_vec_template(
		metric_impl = posLR_impl,
		truth = truth,
		estimate = estimate,
		na_rm = na_rm,
		cls = "factor",
		estimator = estimator,
		...
	)
}

posLR_vec <- yardstick::new_class_metric(posLR_vec, "maximize")

posLR <- function(data, ...){
	UseMethod("posLR")
}

posLR <- new_class_metric(posLR, direction = "maximize")

posLR.data.frame <- function(data, 
														 truth, 
														 estimate, 
														 estimator = NULL, 
														 na_rm = TRUE, 
														 event_level = "first",
														 ...) {
	metric_summarizer(
		metric_nm = "posLR",
		metric_fn = posLR_vec,
		data = data,
		truth = !! enquo(truth),
		estimate = !! enquo(estimate), 
		estimator = estimator,
		na_rm = na_rm,
		event_level = event_level,
		...
	)
}


##NEGATIVE LIKELIHOOD RATIO
negLR_vec <- function(truth, 
											estimate, 
											estimator = NULL, 
											na_rm = TRUE, 
											event_level = "first",
											...) {
	estimator <- finalize_estimator(truth, estimator)
	
	negLR_impl <- function(truth, estimate) {
		# Create 
		xtab <- table(estimate, truth)
		col <- event_col(xtab, event_level)
		col2 <- setdiff(colnames(xtab), col)
		
		tp <- xtab[col, col]
		fp <- xtab[col, col2]
		fn <- xtab[col2, col]
		tn <- xtab[col2, col2]
		
		(1- (tp/(tp + fn)))/(tn/(tn+fp))
	}
	
	metric_vec_template(
		metric_impl = negLR_impl,
		truth = truth,
		estimate = estimate,
		na_rm = na_rm,
		cls = "factor",
		estimator = estimator,
		...
	)
}

negLR_vec <- yardstick::new_class_metric(negLR_vec, "minimize")

negLR <- function(data, ...){
	UseMethod("negLR")
}

negLR <- new_class_metric(negLR, direction = "minimize")

negLR.data.frame <- function(data, 
														 truth, 
														 estimate, 
														 estimator = NULL, 
														 na_rm = TRUE, 
														 event_level = "first",
														 ...) {
	metric_summarizer(
		metric_nm = "negLR",
		metric_fn = negLR_vec,
		data = data,
		truth = !! enquo(truth),
		estimate = !! enquo(estimate), 
		estimator = estimator,
		na_rm = na_rm,
		event_level = event_level,
		...
	)
}


##DIAGNOSTIC ODDS RATIO
DOR_vec <- function(truth, 
										estimate, 
										estimator = NULL, 
										na_rm = TRUE, 
										event_level = "first",
										...) {
	estimator <- finalize_estimator(truth, estimator)
	
	DOR_impl <- function(truth, estimate) {
		# Create 
		xtab <- table(estimate, truth)
		col <- event_col(xtab, event_level)
		col2 <- setdiff(colnames(xtab), col)
		
		tp <- xtab[col, col]
		fp <- xtab[col, col2]
		fn <- xtab[col2, col]
		tn <- xtab[col2, col2]
		
		(tp/fp)/(fn/tn)
	}
	
	metric_vec_template(
		metric_impl = DOR_impl,
		truth = truth,
		estimate = estimate,
		na_rm = na_rm,
		cls = "factor",
		estimator = estimator,
		...
	)
}

DOR_vec <- yardstick::new_class_metric(DOR_vec, "maximize")

DOR <- function(data, ...){
	UseMethod("DOR")
}

DOR <- new_class_metric(DOR, direction = "maximize")

DOR.data.frame <- function(data, 
													 truth, 
													 estimate, 
													 estimator = NULL, 
													 na_rm = TRUE, 
													 event_level = "first",
													 ...) {
	metric_summarizer(
		metric_nm = "DOR",
		metric_fn = DOR_vec,
		data = data,
		truth = !! enquo(truth),
		estimate = !! enquo(estimate), 
		estimator = estimator,
		na_rm = na_rm,
		event_level = event_level,
		...
	)
}


## PREVALENCE
prevalence_vec <- function(truth, 
													 estimate, 
													 estimator = NULL, 
													 na_rm = TRUE, 
													 event_level = "first",
													 ...) {
	estimator <- finalize_estimator(truth, estimator)
	
	prevalence_impl <- function(truth, estimate) {
		# Create 
		
		xtab <- table(estimate, truth)
		col <- event_col(xtab, event_level)
		col2 <- setdiff(colnames(xtab), col)
		
		tp <- xtab[col, col]
		fp <- xtab[col, col2]
		fn <- xtab[col2, col]
		tn <- xtab[col2, col2]
		
		(tp + fn)/(tp + tn + fp + fn)
	}
	
	metric_vec_template(
		metric_impl = prevalence_impl,
		truth = truth,
		estimate = estimate,
		na_rm = na_rm,
		cls = "factor",
		estimator = estimator,
		...
	)
}

prevalence_vec <- yardstick::new_class_metric(prevalence_vec, "maximize")

prevalence <- function(data, ...){
	UseMethod("prevalence")
}

prevalence <- new_class_metric(prevalence, direction = "maximize")

prevalence.data.frame <- function(data, 
																	truth, 
																	estimate, 
																	estimator = NULL, 
																	na_rm = TRUE, 
																	event_level = "first",
																	...) {
	metric_summarizer(
		metric_nm = "prevalence",
		metric_fn = prevalence_vec,
		data = data,
		truth = !! enquo(truth),
		estimate = !! enquo(estimate), 
		estimator = estimator,
		na_rm = na_rm,
		event_level = event_level,
		...
	)
}

## MARKEDNESS
markedness_vec <- function(truth, 
													 estimate, 
													 estimator = NULL, 
													 na_rm = TRUE, 
													 event_level = "first",
													 ...) {
	estimator <- finalize_estimator(truth, estimator)
	
	markedness_impl <- function(truth, estimate) {
		# Create 
		
		xtab <- table(estimate, truth)
		
		col <- event_col(xtab, event_level)
		col2 <- setdiff(colnames(xtab), col)
		
		tp <- xtab[col, col]
		fp <- xtab[col, col2]
		fn <- xtab[col2, col]
		tn <- xtab[col2, col2]
		
		((tp)/(tp+fp) + (tn)/(tn+fn)) - 1
	}
	
	metric_vec_template(
		metric_impl = markedness_impl,
		truth = truth,
		estimate = estimate,
		na_rm = na_rm,
		cls = "factor",
		estimator = estimator,
		...
	)
}

markedness_vec <- yardstick::new_class_metric(markedness_vec, "maximize")

markedness <- function(data, ...){
	UseMethod("markedness")
}

markedness <- new_class_metric(markedness, direction = "maximize")

markedness.data.frame <- function(data, 
																	truth, 
																	estimate, 
																	estimator = NULL, 
																	na_rm = TRUE, 
																	event_level = "first",
																	...) {
	metric_summarizer(
		metric_nm = "markedness",
		metric_fn = markedness_vec,
		data = data,
		truth = !! enquo(truth),
		estimate = !! enquo(estimate), 
		estimator = estimator,
		na_rm = na_rm,
		event_level = event_level,
		...
	)
}

#Create the metric set 
mset <- metric_set(roc_auc, sens, spec, ppv, npv, accuracy,
									 posLR, negLR, DOR, bal_accuracy, mcc,prevalence,
									 detection_prevalence, pr_auc, mn_log_loss,
									 markedness, j_index)



