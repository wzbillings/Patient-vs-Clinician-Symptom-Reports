# Summary function for computing the stratum specific statistics needed
# for recreation of the Ebell-style tables.
# Mostly written by Annika, Updated by Zane
# Moved into this spot 2022-08-30 when Zane realized he needed it in
# more than one place
# These functions are run with an error handler. If no data are passed to
# the function (or something else that is wrong), it will return a
# data frame where all the results are NaN.

stratum_specific_stats <-
	purrr::possibly(
		function(truth, estimate) {
			xtab <- table(estimate, truth)
			#print(xtab)
			#low risk LR
			lowLR = (xtab["Low", "TRUE"] / sum(xtab[ ,"TRUE"])) /
				(xtab["Low", "FALSE"]/sum(xtab[,"FALSE"]))
			#medium risk LR
			medLR = (xtab["Moderate", "TRUE"] / sum(xtab[ ,"TRUE"])) /
				(xtab["Moderate", "FALSE"]/sum(xtab[,"FALSE"]))
			#high risk LR
			highLR = (xtab["High", "TRUE"] / sum(xtab[ ,"TRUE"])) /
				(xtab["High", "FALSE"]/sum(xtab[ ,"FALSE"]))
			
			#percent w flu in low risk group
			lowperc = (xtab["Low", "TRUE"])/sum(xtab["Low", ])
			#percent w flu in med risk group
			medperc = (xtab["Moderate", "TRUE"])/sum(xtab["Moderate", ])
			#percent w flu in high risk group
			highperc = (xtab["High", "TRUE"])/sum(xtab["High", ])
			
			#percent of the whole in each group
			lowfrac =  sum(xtab["Low", ]) / sum(xtab)
			medfrac =  sum(xtab["Moderate", ]) / sum(xtab)
			highfrac = sum(xtab["High", ]) / sum(xtab)
			
			ratio <- c(lowLR, medLR, highLR)
			percgroup <- c(lowfrac, medfrac, highfrac)
			percflu <- c(lowperc, medperc, highperc)
			group <- c("Low", "Moderate", "High")
			return(tibble::tibble(group, ratio, percflu, percgroup))
		},
		otherwise = tibble::tribble(
			~group, ~ratio, ~percflu, ~percgroup,
			"Low", NaN, NaN, NaN,
			"Moderate", NaN, NaN, NaN,
			"High", NaN, NaN, NaN
		)
	)

stratum_specific_counts <-
	purrr::possibly(
		function(truth, estimate) {
			xtab <- table(estimate, truth)
			
			out <- tibble::tribble(
				~group, ~flu_n, ~total_n,
				"Low", xtab["Low", "TRUE"], sum(xtab["Low", ]),
				"Moderate", xtab["Moderate", "TRUE"], sum(xtab["Moderate", ]),
				"High", xtab["High", "TRUE"], sum(xtab["High", ])
			)
			
			return(out)
		},
		otherwise = 
			tibble::tribble(
				~group, ~flu_n, ~total_n,
				"Low", NaN, NaN,
				"Moderate", NaN, NaN,
				"High", NaN, NaN
			)
	)