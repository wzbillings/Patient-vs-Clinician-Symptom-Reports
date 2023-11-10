###
# Generating tables for manuscript
# author: Zane Billings
# date: 2021-08-23
###
box::use(
	gtsummary,
	readr,
	dplyr,
	tidyr,
	flextable,
	strapgod,
	purrr,
	performance
)

# Table 1: Population demographics
pop_df <-
	readr::read_rds(
		here::here("Data", "Clean-Data", "Symptoms.Rds")
	)

long_df <-
	readr::read_rds(
		here::here("Data", "Clean-Data", "Symptoms-Long.Rds")
	)


## Info for desciprtive stats -- sex and age
pop_df |>
	dplyr::select(sex, res_pcr) |>
	tidyr::drop_na() |>
	dplyr::pull(sex) |>
	readr::write_rds(
		file = here::here("Results/Data/sex.Rds")
	)

pop_df |>
	dplyr::select(age, res_pcr) |>
	tidyr::drop_na() |>
	dplyr::pull(age) |>
	readr::write_rds(
		file = here::here("Results/Data/age.Rds")
	)

test_n <- pop_df |>
	dplyr::summarise(
		has_pcr = sum(!is.na(res_pcr)),
		has_radt = sum(!is.na(res_ridt)),
		has_diag = sum(!is.na(res_clinician)),
		sample_size = dplyr::n()
	)

test_n |>
	readr::write_rds(
		file = here::here("Results", "Data", "test-n.Rds")
	)

n_symps <-
	long_df |>
	dplyr::filter(method == "Clinician") |>
	dplyr::group_by(unique_visit, rater) |>
	dplyr::summarize(num_symps = sum(present), .groups = "drop") |>
	tidyr::pivot_wider(names_from = rater, values_from = num_symps) |>
	dplyr::rename(
		"pt_number of symptoms" = patient,
		"cl_number of symptoms" = clinician
	)

pop_df2 <-
	dplyr::left_join(
		pop_df,
		n_symps,
		by = c("unique_visit")
	)

pop_tab_1 <-
	pop_df2 |>
	dplyr::select(tidyselect::starts_with("res"), flu_type, unique_visit,
								sex, age, tidyselect::ends_with("symptoms")) |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("res"),
		names_to = "method",
		values_to = "diagnosis",
		names_prefix = "res_"
	) |>
	dplyr::mutate(
		method = dplyr::if_else(is.na(diagnosis), NA_character_, method),
		method = factor(
			method,
			levels = c("clinician", "pcr", "ridt", "any_test"),
			labels = c("Clinician", "PCR", "RIDT", "Any test")
		),
		diagnosis = factor(
			diagnosis,
			levels = c(FALSE, TRUE),
			labels = c("negative", "positive")
		),
		flu_type2 = dplyr::case_when(
			method == "Clinician" & diagnosis == "positive" ~ NA_character_,
			method == "Clinician" & diagnosis == "negative" ~ NA_character_,
			is.na(flu_type) ~ NA_character_,
			TRUE ~ as.character(flu_type)
		),
		flu_type2 = dplyr::if_else(diagnosis == "negative", "negative", flu_type2),
		flu_type2 = factor(
			flu_type2,
			levels = c("A", "B", "negative"),
			labels = c("Influenza A positive", "Influenza B positive", "negative")
		)
	) |>
	dplyr::filter(method == "PCR") |>
	tidyr::drop_na(diagnosis) |>
	gtsummary::tbl_summary(
		include = c(flu_type2, sex, age),
		label = list(
			flu_type2 ~ "Detected flu type",
			sex ~ "Sex",
			age ~ "Age"
		),
		digits = list(age ~ 0),
		statistic = list(gtsummary::all_continuous() ~ "{median} ({min}, {max})"),
		missing = "no"
	) |>
	gtsummary::modify_footnote(
		gtsummary::all_stat_cols() ~ "n (%)"
	) |>
	gtsummary::modify_table_styling(
		columns = label,
		rows = label %in% c(
			"Age",
			"Number of symptoms reported by patient",
			"Number of symptoms reported by clinician"
		),
		footnote = "median (range)"
	); pop_tab_1

readr::write_rds(pop_tab_1, file = here::here("Results", "Tables", "table_one.Rds"))

# Symptoms prevalence tables ####

## Data processing ####
symp_tbl_data <-
	long_df |>
	dplyr::mutate(
		symptom = forcats::fct_rev(symptom)
	) |>
	# Get total # symps reported
	dplyr::group_by(unique_visit, method, rater) |>
	dplyr::mutate(n_symps = sum(present)) |>
	dplyr::ungroup() |>
	dplyr::arrange(dplyr::desc(symptom)) |>
	tidyr::pivot_wider(
		names_from = symptom,
		values_from = present
	) |>
	dplyr::rowwise() |>
	dplyr::ungroup() |>
	dplyr::select(
		-sex, -age
	)

symp_tbl_data2 <-
	symp_tbl_data |>
	dplyr::bind_rows(
		symp_tbl_data |> dplyr::mutate(diagnosis = "overall")
	)

## Prevalence table non-stratified ####
symp_tbl <-
	symp_tbl_data |>
	dplyr::filter(method == "PCR") |>
	gtsummary::tbl_summary(
		by = rater,
		include = -c(unique_visit, diagnosis, method, flu_type),
		label = list(n_symps ~ "Number of symptoms"),
		statistic = list(gtsummary::all_continuous() ~ "{median} ({min}, {max})")
	) |>
	gtsummary::modify_footnote(
		gtsummary::all_stat_cols() ~ "n (%)"
	) |>
	gtsummary::modify_table_styling(
		columns = label,
		rows = label == "Number of symptoms",
		footnote = "median (range)"
	)

## Prevalence table stratified by outcome ####
symp_strat <-
	symp_tbl_data2 |>
	dplyr::filter(method == "PCR") |>
	dplyr::select(-method, -unique_visit, -flu_type) |>
	dplyr::mutate(
		diagnosis = factor(
			diagnosis,
			levels = c("positive", "negative", "overall"),
			labels = c("Influenza +", "Influenza -", "Overall")
		),
		rater = factor(
			rater,
			levels = c("clinician", "patient"),
			labels = c("Clinician", "Patient")
		)
	) |>
	gtsummary::tbl_strata(
		strata = diagnosis,
		.tbl_fun =
			~ .x |>
			gtsummary::tbl_summary(
				by = rater,
				label = list(n_symps ~ "Total number of symptoms"),
				statistic = list(gtsummary::all_continuous() ~ "{median} ({min}, {max})")
			) |>
			gtsummary::modify_header(
				gtsummary::all_stat_cols() ~ "{level}"
			) |>
			gtsummary::modify_footnote(
				gtsummary::all_stat_cols() ~ NA
			),
		.header = "**{strata}**\n(N = {n / 2})"
	) |>
	gtsummary::modify_header(
		label ~ ""
	)

readr::write_rds(symp_strat, file = here::here("Results", "Tables",
																							 "pcr_symptom_prevalence.Rds"))

# Performance of prior CDR tables ####

## Prior reports ####

# Numbers for SM and TM taken from validation group in original papers.
#  - For SM, Van Vugt external validation 2015
#  - For TM, Afonso 2012
# AUROCC for SM is from Ebell systematic review and is listed as all patients,
# but does not occur in the original paper.

sens_spec <-
	tibble::tribble(
		~CDR, ~symptoms, ~sens, ~spec,
		"CF", "cough, fever", 63.81, 67.19,
		"CFA", "cough, fever, acute onset", 63.32, 67.54,
		"CFM", "cough, fever, myalgia", 61.50, 68.52
	) |>
	dplyr::mutate(
		sens = sens / 100,
		spec = spec / 100,
		lr_high = sens / (1 - spec),
		lr_low = (1 - sens) / spec,
		AUROCC = (sens + spec) / 2,
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 2))
	) |>
	dplyr::select(-sens, -spec) |>
	tidyr::pivot_longer(
		cols = tidyselect::starts_with("lr_"),
		names_to = "group",
		names_prefix = "lr_",
		values_to = "LR"
	)

prev_score_dat <-
	tibble::tribble(
		~CDR, ~symptoms, ~group, ~AUROCC, ~LR, ~pct_flu, ~pct_group,
		"SM", "fever and cough, acute onset, myalgia, chills/sweats",  "low", 0.71,
		0.51, 13.6, 60.0,
		"SM", "fever and cough, acute onset, myalgia, chills/sweats",  "med", 0.71,
		1.53, 32.1, 27.0,
		"SM", "fever and cough, acute onset, myalgia, chills/sweats", "high", 0.71,
		3.24, 50.0, 14.0,
		"TM", "fever, acute onset, cough, chills/sweats",  "low", 0.80, 0.15, 08.0,
		19.0,
		"TM", "fever, acute onset, cough, chills/sweats",  "med", 0.80, 0.98, 37.0,
		68.0,
		"TM", "fever, acute onset, cough, chills/sweats", "high", 0.80, 7.80, 82.0,
		13.0
	)

all_prev <-
	dplyr::bind_rows(sens_spec, prev_score_dat) |>
	dplyr::transmute(
		CDR, "Symptoms" = symptoms, AUROCC,
		"Risk Group" = factor(
			group,
			levels = c("low", "med", "high"),
			labels = c("Low", "Mod", "High"),
			ordered = TRUE
		),
		LR,
		"% Flu" = dplyr::if_else(
			is.na(pct_flu),
			NA_character_,
			paste0(sprintf("%.1f", pct_flu), "%")
		),
		"% Patients in Group" = dplyr::if_else(
			is.na(pct_group),
			NA_character_,
			paste0(sprintf("%.1f", pct_group), "%")
		),
	) |>
	dplyr::arrange(CDR, `Risk Group`)

CDRs_prev_tab <-	
	all_prev |>
	flextable::flextable() |>
	flextable::merge_v(~CDR + Symptoms + AUROCC, combine = TRUE) |>
	flextable::align(
		j = c(3, 5, 6, 7),
		align = "center",
		part = "all"
	) |>
	flextable::fix_border_issues()

readr::write_rds(CDRs_prev_tab, here::here("Results", "Tables", "CDR_prev.Rds"))

## Clinician ####
score_stat_df <-
	readr::read_rds(
		here::here("Results", "CPR-Assessment-Data.Rds")
	)

pcr_cl_chart <-
	score_stat_df |>
	dplyr::filter(rater == "Clinician") |>
	dplyr::select(-rater) |>
	dplyr::filter(method == "pcr") |>
	dplyr::select(-method) |>
	flextable::flextable(
		col_keys = c("score", "auc", "lr_col", "pp_col")
	) |>
	flextable::set_header_labels(
		values = list(score = "CPR", auc = "AUROCC",
									lr_col = "% Flu and LR by Risk Group",
									pp_col = "% of Patients in Each Risk Group")
	) |>
	flextable::merge_v(j = ~ auc + score) |>
	flextable::valign(j = ~ auc + score, valign = "top") |>
	flextable::colformat_double(digits = 2) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	pcr_cl_chart,
	here::here("Results", "Tables", "CDR-Perf-Cl-Data.Rds")
)

## Patient ####
pcr_pt_chart <-
	score_stat_df |>
	dplyr::filter(rater == "Patient") |>
	dplyr::select(-rater) |>
	dplyr::filter(method == "pcr") |>
	dplyr::select(-method) |>
	flextable::flextable(
		col_keys = c("score", "auc", "lr_col", "pp_col")
	) |>
	flextable::set_header_labels(
		values = list(score = "CPR", auc = "AUROCC",
									lr_col = "% Flu and LR by Risk Group",
									pp_col = "% of Patients in Each Risk Group")
	) |>
	flextable::merge_v(j = ~ auc + score) |>
	flextable::valign(j = ~ auc + score, valign = "top") |>
	flextable::colformat_double(digits = 2) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	pcr_pt_chart,
	here::here("Results", "Tables", "CDR-Perf-Pt-Data.Rds")
)

## AUC table for manuscript ####
# Combine the AUC parts of the three tables and rename appropriately
cl_pt_auc <-
	score_stat_df |>
	dplyr::filter(method == "pcr") |>
	dplyr::select(rater, CDR = score, auc) |>
	dplyr::distinct() |>
	# Manually add source column
	dplyr::mutate(
		source = c(
			rep("Monto 2000", times = 6),
			rep("van Vugt 2015", times = 2),
			rep("Afonso 2012", times =2)
		),
		.after = rater
	) |>
	tidyr::pivot_wider(
		names_from = rater,
		values_from = auc
	) |>
	dplyr::mutate(
		CDR = dplyr::recode(
			CDR,
			CF = "CF",
			CFA = "CFA",
			CFM = "CFM",
			Score = "WS",
			Tree = "TM"
		),
		CDR = as.character(CDR)
	)

prev_auc <-
	all_prev |>
	dplyr::select(
		CDR, Symptoms, Previous = AUROCC 
	) |>
	dplyr::mutate(
		CDR = dplyr::recode(
			CDR,
			SM = "WS"
		)
	) |>
	dplyr::distinct()

CDR_val_data <-
	dplyr::inner_join(prev_auc, cl_pt_auc, by = "CDR") |>
	dplyr::select(
		CDR, Symptoms, "Source" = source,
		"Previously reported" = Previous,
		"Clinician-reported symptoms" = Clinician,
		"Patient-reported symptoms" = Patient
	)

# Make it a flextable and save to disk
CDR_tab <-
	CDR_val_data |>
	flextable::flextable() |>
	flextable::align(j = 4:6, align = "center", part = "body") |>
	flextable::colformat_double(digits = 2) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	CDR_tab,
	here::here("Results", "Tables", "CDR-Assessment.Rds")
)

# Contingency table for heuristic rules ####
long_score_dat <- readRDS(here::here("Results/Data/long_score_dat.Rds"))

score_data_1 <-
	long_score_dat |>
	dplyr::filter(CPR %in% c("CF", "CFA", "CFM")) |>
	dplyr::filter(method == "pcr") |>
	tidyr::pivot_longer(
		cols = c(pt, cl),
		names_to = "rater",
		values_to = "score"
	)

# Add a fake dataset to trick gtsummary into making an overall column
score_data_2 <-
	dplyr::bind_rows(
		dplyr::mutate(
			score_data_1,
			diagnosis = as.character(diagnosis)
		),
		dplyr::mutate(
			score_data_1,
			diagnosis = "overall"
		)
	)

score_data_3 <-
	score_data_1 |>
	dplyr::mutate(
		diagnosis = factor(
			diagnosis,
			levels = c("TRUE", "FALSE", "overall"),
			labels = c("Influenza +", "Influenza -", "Overall")
		) |> forcats::fct_drop(),
		rater = factor(
			rater,
			levels = c("cl", "pt"),
			labels = c("Clinician", "Patient")
		)
	) |>
	tidyr::pivot_wider(
		names_from = CPR,
		values_from = score
	) |>
	dplyr::mutate(
		dplyr::across(
			c(CF, CFA, CFM),
			\(x) factor(
				x,
				levels = c(1, 0),
				labels = c("Positive", "Negative")
			)
		)
	) |>
	dplyr::select(-method, -unique_visit)

heuristics_contigency_table <-
	score_data_3 |>
	gtsummary::tbl_strata(
		strata = rater,
		.tbl_fun =
			~ .x |>
			gtsummary::tbl_summary(
				by = diagnosis,
				type = list(gtsummary::everything() ~ "categorical"),
				value = list(gtsummary::everything() ~ "Positive")
			) |>
			gtsummary::modify_header(
				gtsummary::all_stat_cols() ~ "{level}"
			) |>
			gtsummary::modify_footnote(
				gtsummary::all_stat_cols() ~ NA
			),
		.header = "**{strata}**\n(N = {n})"
	) |>
	gtsummary::modify_header(
		label ~ ""
	) |>
	gtsummary::as_flex_table()

flextable::autofit(heuristics_contigency_table)

heuristics_contigency_table |>
	readr::write_rds(
		here::here("Results", "Tables", "heuristics_contigency_table.Rds")
	)

# Table of symptom correlation with diagnosis ####
boot_corrs <-
	readRDS(here::here("Data", "Clean-Data", "Symptoms-Long.Rds")) |>
	dplyr::filter(method == "PCR") |>
	dplyr::select(-method, diagnosis = diagnosis) |>
	dplyr::mutate(present = factor(present, levels = c(FALSE, TRUE),
																 labels = c("negative", "positive"))) |>
	dplyr::group_by(rater, symptom) |>
	strapgod::bootstrapify(times = 10000) |>
	dplyr::collect() |>
	dplyr::summarize(
		tbl = list(table(diagnosis, present)),
		.groups = "drop_last"
	) |>
	dplyr::mutate(
		r = purrr::map_dbl(
			tbl,
			~rcompanion::cramerV(x = .x, bias.correct = TRUE)
		)
	) |>
	dplyr::summarize(
		estimate = mean(r, na.rm = TRUE),
		ymin = quantile(r, probs = 0.025, na.rm = TRUE),
		ymax = quantile(r, probs = 0.975, na.rm = TRUE),
		.groups = "drop"
	) |>
	dplyr::mutate(
		dplyr::across(c(estimate, ymin, ymax), ~format(round(.x, 2), nsmall = 2)),
		dplyr::across(c(estimate, ymin, ymax), ~ifelse(.x == "-0.00", "0.00", .x)),
		corr = paste0(estimate, " (", ymin, ", ", ymax, ")")
	) |>
	dplyr::select(-estimate, -ymin, -ymax) |>
	tidyr::pivot_wider(names_from = rater, values_from = corr)

readr::write_rds(boot_corrs, file = here::here("Results", "Tables", "symp_cramer_v.Rds"))

# Correlations table for score agreement ####
sa_dat <- readRDS(here::here("Results/Data/num_cpr_agreement.Rds"))

pcr_corr_tab <- 
	sa_dat |>
	dplyr::filter(method == "pcr") |>
	dplyr::mutate(dplyr::across(c(estimate, lower, upper),
															~sprintf("%.3f", round(.x, 3)))) |>
	dplyr::transmute(
		CPR, statistic,
		val = paste0(estimate, "; 95% CI: (", lower, ", ", upper, ")")
	) |>
	tidyr::pivot_wider(
		names_from = statistic,
		values_from = val
	) |>
	dplyr::rename(
		"Spearman correlation" = rho,
		"Intraclass correlation" = ICC
	)

readr::write_rds(pcr_corr_tab, file = here::here("Results", "Tables", "pcr_score_corrs.Rds"))

# Score model tables ####

## Patient-data models ####
glm_list <- readr::read_rds(here::here("Results", "Data", "Pt",
																			 "Score-Models", "Score-GLMs.Rds"))

names(glm_list) <-
	c(
		"A priori symptom score",
		"LASSO score",
		"Re-fit FluScore model (Ebell 2012)",
		"Cough/fever heuristic",
		"Cough/fever/acute onset heuristic",
		"Cough/fever/myalgia heuristic",
		"LASSO heuristic",
		"Cough/fever symptom score",
		"Cough/fever/acute onset symptom score",
		"CFM (weighted)" = "Cough/fever/myalgia symptom score"
	)

### Table of coefficients for each model ####
score_coefs_tab <-
	glm_list |>
	purrr::map(gtsummary::tbl_regression) |>
	gtsummary::tbl_stack(group_header = names(glm_list)) |>
	gtsummary::modify_table_styling(columns = p.value, hide = TRUE) |>
	gtsummary::as_flex_table() |>
	flextable::set_header_labels(
		groupname_col = "Score model",
		label = "Symptom",
		estimate = "Beta"
	)

readr::write_rds(
	score_coefs_tab,
	here::here("Results", "Tables", "All-Score-Coefs.Rds")
)

### Table with Points ####
pt_with_pts <-
	readr::read_rds(
		here::here("Results", "Tables", "Score-Model-Points-Tab-Pt.Rds")
	)

score_pts_tab_pt <-
	pt_with_pts |>
	dplyr::mutate(
		dplyr::across(
			c(coefficient, lwr, upr),
			\(x) sprintf("%.2f", x)
		),
		"95% CI" = paste0(lwr, ", ", upr)
	) |>
	dplyr::select(
		-lwr, -upr,
		"Score model" = score,
		"Symptom" = term,
		"b" = coefficient,
		"Points" = pts
	) |>
	flextable::flextable() |>
	flextable::merge_v(j = ~`Score model`) |>
	flextable::valign(j = 1, valign = "top")

readr::write_rds(
	score_pts_tab_pt,
	here::here("Results", "Tables", "Score-Points-Final-Pt.Rds")
)

### Table with only lasso/cfs model coefficients ####
cfs_coefs_tab <-
	glm_list[["LASSO score"]] |>
	broom:::tidy.glm(conf.int = TRUE, conf.level = 0.95) |>
	dplyr::filter(term != "(Intercept)") |>
	dplyr::mutate(
		# Get the points by rounding to the nearest 1/2
		pts = as.character(round((estimate / 0.5))),
		# Format the numeric variables for the table
		dplyr::across(tidyselect:::where(is.numeric), ~sprintf("%.2f", .x)),
		ci = paste0(estimate, " (", conf.low, ", ", conf.high, ")"),
	) |>
	dplyr::select(
		Symptom = term,
		"Beta (95% CI)" = ci,
		"Rounded point value" = pts
	) |>
	dplyr::mutate(
		Symptom = forcats::fct_recode(
			Symptom,
			"Chills/Sweats" = "chills_sweats",
			"Cough" = "cough",
			"Subjective fever" = "subjective_fever"
		)
	) |>
	flextable::flextable() |>
	flextable::align(j = -1, align = "center")

readr::write_rds(
	cfs_coefs_tab,
	here::here("Results", "Tables", "CFS-Score-Coefs.Rds")
)

### Table with point to risk conversion ####
risks_raw <-
	readr::read_rds(here::here("Results", "Data", "Pt",
														 "Score-Models", "Scores-to-Risks-Table.Rds"))

risks <-
	risks_raw |>
	dplyr::rename(model = score) |>
	dplyr::group_by(model) |>
	dplyr::mutate(
		dplyr::across(
			c(fit, lwr, upr),
			~paste0(sprintf("%.0f", .x * 100), "%")
		)
	) |>
	dplyr::transmute(
		"Score" = value,
		"Risk (95% CI)" = paste0(fit, " (", lwr, ", ", upr, ")")
	)

risks_tab <-
	risks |>
	ftExtra::as_flextable(groups_to = "merged")

readr::write_rds(
	risks_tab,
	here::here("Results", "Tables", "CFS-Score-Risks.Rds")
)

### Table of metrics for each score model ####
aic_tab <-
	glm_list |>
	performance::compare_performance()

aic_table <-
	aic_tab |>
	dplyr::arrange(AIC, BIC) |>
	dplyr::transmute(
		Name, AIC, BIC,
		"Tjur R^2" = R2_Tjur,
		"Brier score" = RMSE ^ 2) |>
	flextable::flextable() |>
	flextable::colformat_double(digits = 2)

readr::write_rds(
	aic_table,
	here::here("Results", "Tables", "Score-AICs.Rds")
)

## Clinician-data models ####
glm_list_cl <- readr::read_rds(here::here("Results", "Data", "Cl",
																					"Score-Models", "Score-GLMs.Rds"))

names(glm_list_cl) <- names(glm_list)

### Table of coefficients for each model ####
score_coefs_tab <-
	glm_list_cl |>
	purrr::map(gtsummary::tbl_regression) |>
	gtsummary::tbl_stack(group_header = names(glm_list_cl)) |>
	gtsummary::modify_table_styling(columns = p.value, hide = TRUE) |>
	gtsummary::as_flex_table() |>
	flextable::set_header_labels(
		groupname_col = "Score model",
		label = "Symptom",
		estimate = "b"
	)

readr::write_rds(
	score_coefs_tab,
	here::here("Results", "Tables", "All-Score-Coefs-Cl.Rds")
)

### Table with Points ####
cl_with_cls <-
	readr::read_rds(
		here::here("Results", "Tables", "Score-Model-Points-Tab-Cl.Rds")
	)

score_cls_tab_cl <-
	cl_with_cls |>
	dplyr::mutate(
		dplyr::across(
			c(coefficient, lwr, upr),
			\(x) sprintf("%.2f", x)
		),
		"95% CI" = paste0(lwr, ", ", upr)
	) |>
	dplyr::select(
		-lwr, -upr,
		"Score model" = score,
		"Symptom" = term,
		"b" = coefficient,
		"Points" = pts
	) |>
	flextable::flextable() |>
	flextable::merge_v(j = ~`Score model`) |>
	flextable::valign(j = 1, valign = "top")

readr::write_rds(
	score_cls_tab_cl,
	here::here("Results", "Tables", "Score-Points-Final-Cl.Rds")
)

### Table with only lasso/cfs model coefficients ####
cfs_coefs_tab <-
	glm_list_cl[["LASSO score"]] |>
	broom:::tidy.glm(conf.int = TRUE, conf.level = 0.95) |>
	dplyr::filter(term != "(Intercept)") |>
	dplyr::mutate(
		# Get the points by rounding to the nearest 1/2
		pts = as.character(round((estimate / 0.5))),
		# Format the numeric variables for the table
		dplyr::across(tidyselect:::where(is.numeric), ~sprintf("%.2f", .x)),
		ci = paste0(estimate, " (", conf.low, ", ", conf.high, ")"),
	) |>
	dplyr::select(
		Symptom = term,
		"Beta (95% CI)" = ci,
		"Rounded point value" = pts
	) |>
	dplyr::mutate(
		Symptom = forcats::fct_recode(
			Symptom,
			"Chills/Sweats" = "chills_sweats",
			"Myalgia" = "myalgia",
			"Subjective fever" = "subjective_fever",
			"Runny nose" = "runny_nose",
			"Eye pain" = "eye_pain",
			"Swollen lymph nodes" = "swollen_lymph_nodes"
		)
	) |>
	flextable::flextable() |>
	flextable::align(j = -1, align = "center")

readr::write_rds(
	cfs_coefs_tab,
	here::here("Results", "Tables", "CFS-Score-Coefs-Cl.Rds")
)

### Table with point to risk conversion ####
risks_raw <-
	readr::read_rds(here::here("Results", "Data", "Cl",
														 "Score-Models", "Scores-to-Risks-Table.Rds"))

risks <-
	risks_raw |>
	dplyr::rename(model = score) |>
	dplyr::group_by(model) |>
	dplyr::mutate(
		dplyr::across(
			c(fit, lwr, upr),
			~paste0(sprintf("%.0f", .x * 100), "%")
		)
	) |>
	dplyr::transmute(
		"Score" = value,
		"Risk (95% CI)" = paste0(fit, " (", lwr, ", ", upr, ")")
	)

risks_tab <-
	risks |>
	ftExtra::as_flextable(groups_to = "merged")

readr::write_rds(
	risks_tab,
	here::here("Results", "Tables", "CFS-Score-Risks-Cl.Rds")
)

### Table of metrics for each score model ####
aic_tab <-
	glm_list_cl |>
	performance::compare_performance()

aic_table <-
	aic_tab |>
	dplyr::arrange(AIC, BIC) |>
	dplyr::transmute(
		Name, AIC, BIC,
		"Tjur R^2" = R2_Tjur,
		"Brier score" = RMSE ^ 2) |>
	flextable::flextable() |>
	flextable::colformat_double(digits = 2)

readr::write_rds(
	aic_table,
	here::here("Results", "Tables", "Score-AICs-Cl.Rds")
)

# AUROCC Tables for our models ####

perf_AUCs <- readr::read_rds(here::here("Results", "Data", "Model-AUCs.rds"))

AUCs_tab_data <-
	perf_AUCs |>
	dplyr::select(
		model, sample, rater, auc
	) |>
	dplyr::filter(
		sample != "Cross-validated"
	) |>
	tidyr::pivot_wider(
		names_from = c(sample, rater),
		values_from = auc
	)

## Short table for manuscript ####
model_perf_tab <-
	AUCs_tab_data |>
	dplyr::filter(
		model %in% c("LASSO", "Conditional inference", "nb")
	) |>
	dplyr::mutate(
		model = dplyr::recode(
			model,
			"LASSO" = "LASSO point score",
			"Conditional inference" = "Conditional inference tree",
			"nb" = "Naive Bayes classifier",
		)
	) |>
	dplyr::relocate(
		model,
		`Derivation group_clinician`,
		`Derivation group_patient`,
		`Validation group_clinician`,
		`Validation group_patient`
	) |>
	dplyr::rename(" " = model) |>
	flextable::flextable() |>
	flextable::separate_header(split = "_") |>
	flextable::align(
		j = 1,
		align = "left",
		part = "all"
	) |>
	flextable::align(
		j = 2:5,
		align = "center",
		part = "all"
	) |>
	flextable::colformat_double(digits = 2)

readr::write_rds(
	model_perf_tab,
	here::here("Results", "Tables", "Model-AUC-Tab.Rds")
)

## Full version for Supplement ####
model_perf_tab_sm <-
	AUCs_tab_data |>
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
		)
	)|>
	# TODO recode all variable names
	dplyr::relocate(
		model,
		`Derivation group_clinician`,
		`Derivation group_patient`,
		`Validation group_clinician`,
		`Validation group_patient`
	) |>
	dplyr::rename(" " = model) |>
	flextable::flextable() |>
	flextable::separate_header(split = "_") |>
	flextable::align(
		j = 1,
		align = "left",
		part = "all"
	) |>
	flextable::align(
		j = 2:5,
		align = "center",
		part = "all"
	) |>
	flextable::colformat_double(digits = 2)

readr::write_rds(
	model_perf_tab_sm,
	here::here("Results", "Tables", "Supplement-AUC-Tab.Rds")
)

# Risk group (stratum specific) statistic tables ####

rg_data <-
	readr::read_rds(
		here::here("Results", "Data", "Stratum-Specific-Stats.Rds")
	)

thresholds_data <-
	readr::read_rds(
		here::here("Results", "Data", "risk-group-thresholds-data.Rds")
	) |>
	dplyr::mutate(
		dplyr::across(
			.cols = c(ratio, percflu),
			~ifelse(is.nan(.x), NA_real_, .x)
		),
		col1 = paste0(flu_n, "/", total_n, " (",
									sprintf("%.1f", percflu * 100), ")"),
		col2 = sprintf("%.1f", ratio),
		col3 = sprintf("%.1f", percgroup * 100),
		group = factor(
			group,
			levels = c("Low", "Moderate", "High"),
			ordered = TRUE
		)
	) |>
	dplyr::arrange(sample, model_group, model, group) |>
	dplyr::select(
		thresholds, rater, sample, model_group, model, group,
		"Flu/Total (%)" = col1,
		"LR" = col2,
		"In Group (%)" = col3
	)

# Data cleaning
rg_tab_data <-
	rg_data |>
	dplyr::mutate(
		rater = factor(rater),
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
		) |> factor(),
		model_group = factor(
			model_group,
			levels = c("point score", "decision tree", "machine learning"),
			labels = c("Score", "Tree", "ML")
		),
		group = factor(
			group,
			levels = c("Low", "Moderate", "High"),
			ordered = TRUE
		)
	) |>
	dplyr::mutate(
		dplyr::across(
			.cols = c(ratio, percflu),
			~ifelse(is.nan(.x), NA_real_, .x)
		),
		col1 = paste0(flu_n, "/", total_n, " (",
									sprintf("%.1f", percflu * 100), ")"),
		col2 = sprintf("%.1f", ratio),
		col3 = sprintf("%.1f", percgroup * 100)
	) |>
	dplyr::arrange(sample, model_group, model, group) |>
	dplyr::select(
		rater, sample, model_group, model, group,
		"Flu/Total (%)" = col1,
		"LR" = col2,
		"In Group (%)" = col3
	)

## Patient data ####

### Main text table ####

pt_main_tab <-
	rg_tab_data |>
	dplyr::filter(
		rater == "patient",
		model %in% c("LASSO score",
								 "Conditional inference tree (manual)",
								 "Naive Bayes classifier")
	) |>
	dplyr::select(-rater, -model_group) |>
	tidyr::pivot_wider(
		id_cols = c(model, group),
		names_from = sample,
		names_glue = "{sample}.{.value}",
		values_from = -c(sample, model, group)
	) |>
	dplyr::select(
		model, " " = group,
		starts_with("derivation"),
		starts_with("validation")
	) |>
	dplyr::group_by(model) |>
	ftExtra::as_flextable(
		groups_to = "titles",
		hide_grouplabel = TRUE,
		groups_arrange = FALSE
	) |>
	flextable::separate_header(
		split = ".",
		fixed = TRUE
	) |>
	flextable::padding(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = 1,
		padding.left = 20,
		part = "body"
	) |>
	flextable::align(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "body"
	) |>
	flextable::align(
		i = 2,
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "header"
	) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	pt_main_tab,
	here::here("Results", "Tables", "Risk-Groups-Table-Pt.Rds")
)

### Threshold analysis 25/60 ####
pt_2560 <-
	thresholds_data |>
	dplyr::filter(
		rater == "patient",
		model %in% c("LASSO score",
								 "Conditional inference tree (manual)",
								 "Naive Bayes classifier"),
		thresholds == "25/60"
	) |>
	dplyr::select(-rater, -model_group, -thresholds) |>
	tidyr::pivot_wider(
		id_cols = c(model, group),
		names_from = sample,
		names_glue = "{sample}.{.value}",
		values_from = -c(sample, model, group)
	) |>
	dplyr::select(
		model, " " = group,
		starts_with("derivation"),
		starts_with("validation")
	) |>
	dplyr::group_by(model) |>
	ftExtra::as_flextable(
		groups_to = "titles",
		hide_grouplabel = TRUE,
		groups_arrange = FALSE
	) |>
	flextable::separate_header(
		split = ".",
		fixed = TRUE
	) |>
	flextable::padding(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = 1,
		padding.left = 20,
		part = "body"
	) |>
	flextable::align(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "body"
	) |>
	flextable::align(
		i = 2,
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "header"
	) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	pt_2560,
	here::here("Results", "Tables", "threshold-2560-pt.Rds")
)


### Threshold analysis 30/70 ####
pt_3070 <-
	thresholds_data |>
	dplyr::filter(
		rater == "patient",
		model %in% c("LASSO score",
								 "Conditional inference tree (manual)",
								 "Naive Bayes classifier"),
		thresholds == "30/70"
	) |>
	dplyr::select(-rater, -model_group, -thresholds) |>
	tidyr::pivot_wider(
		id_cols = c(model, group),
		names_from = sample,
		names_glue = "{sample}.{.value}",
		values_from = -c(sample, model, group)
	) |>
	dplyr::select(
		model, " " = group,
		starts_with("derivation"),
		starts_with("validation")
	) |>
	dplyr::group_by(model) |>
	ftExtra::as_flextable(
		groups_to = "titles",
		hide_grouplabel = TRUE,
		groups_arrange = FALSE
	) |>
	flextable::separate_header(
		split = ".",
		fixed = TRUE
	) |>
	flextable::padding(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = 1,
		padding.left = 20,
		part = "body"
	) |>
	flextable::align(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "body"
	) |>
	flextable::align(
		i = 2,
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "header"
	) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	pt_3070,
	here::here("Results", "Tables", "threshold-3070-pt.Rds")
)

## Clinician data ####

### Main appendix table ####

cl_main_tab <-
	rg_tab_data |>
	dplyr::filter(
		rater == "clinician",
		model %in% c("LASSO score",
								 "Conditional inference tree (manual)",
								 "Naive Bayes classifier")
	) |>
	dplyr::select(-rater, -model_group) |>
	tidyr::pivot_wider(
		id_cols = c(model, group),
		names_from = sample,
		names_glue = "{sample}.{.value}",
		values_from = -c(sample, model, group)
	) |>
	dplyr::select(
		model, " " = group,
		starts_with("derivation"),
		starts_with("validation")
	) |>
	dplyr::group_by(model) |>
	ftExtra::as_flextable(
		groups_to = "titles",
		hide_grouplabel = TRUE,
		groups_arrange = FALSE
	) |>
	flextable::separate_header(
		split = ".",
		fixed = TRUE
	) |>
	flextable::padding(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = 1,
		padding.left = 20,
		part = "body"
	) |>
	flextable::align(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "body"
	) |>
	flextable::align(
		i = 2,
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "header"
	) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	cl_main_tab,
	here::here("Results", "Tables", "Risk-Groups-Table-Cl.Rds")
)

### Threshold analysis 25/60 ####
cl_2560 <-
	thresholds_data |>
	dplyr::filter(
		rater == "clinician",
		model %in% c("LASSO score",
								 "Conditional inference tree (manual)",
								 "Naive Bayes classifier"),
		thresholds == "25/60"
	) |>
	dplyr::select(-rater, -model_group, -thresholds) |>
	tidyr::pivot_wider(
		id_cols = c(model, group),
		names_from = sample,
		names_glue = "{sample}.{.value}",
		values_from = -c(sample, model, group)
	) |>
	dplyr::select(
		model, " " = group,
		starts_with("derivation"),
		starts_with("validation")
	) |>
	dplyr::group_by(model) |>
	ftExtra::as_flextable(
		groups_to = "titles",
		hide_grouplabel = TRUE,
		groups_arrange = FALSE
	) |>
	flextable::separate_header(
		split = ".",
		fixed = TRUE
	) |>
	flextable::padding(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = 1,
		padding.left = 20,
		part = "body"
	) |>
	flextable::align(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "body"
	) |>
	flextable::align(
		i = 2,
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "header"
	) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	cl_2560,
	here::here("Results", "Tables", "threshold-2560-cl.Rds")
)

### Threshold analysis 30/70 ####
cl_3070 <-
	thresholds_data |>
	dplyr::filter(
		rater == "clinician",
		model %in% c("LASSO score",
								 "Conditional inference tree (manual)",
								 "Naive Bayes classifier"),
		thresholds == "30/70"
	) |>
	dplyr::select(-rater, -model_group, -thresholds) |>
	tidyr::pivot_wider(
		id_cols = c(model, group),
		names_from = sample,
		names_glue = "{sample}.{.value}",
		values_from = -c(sample, model, group)
	) |>
	dplyr::select(
		model, " " = group,
		starts_with("derivation"),
		starts_with("validation")
	) |>
	dplyr::group_by(model) |>
	ftExtra::as_flextable(
		groups_to = "titles",
		hide_grouplabel = TRUE,
		groups_arrange = FALSE
	) |>
	flextable::separate_header(
		split = ".",
		fixed = TRUE
	) |>
	flextable::padding(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = 1,
		padding.left = 20,
		part = "body"
	) |>
	flextable::align(
		i = c(2, 3, 4, 6, 7, 8, 10, 11, 12),
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "body"
	) |>
	flextable::align(
		i = 2,
		j = c(2, 3, 4, 5, 6, 7),
		align = "center",
		part = "header"
	) |>
	flextable::line_spacing(space = 0.75, part = "all") |>
	flextable::fix_border_issues()

readr::write_rds(
	cl_3070,
	here::here("Results", "Tables", "threshold-3070-cl.Rds")
)

# Clinician/PCR contigency table ####

# the 2x2 table of clinician vs PCR diagnoses for the supplement
cl_pcr_tab <-
	long_df |>
	dplyr::filter(method == "Clinician" | method == "PCR") |>
	dplyr::select(unique_visit, method, diagnosis) |>
	dplyr::distinct() |>
	# Pivot wider so we can A) compare B) filter for PCR subset
	tidyr::pivot_wider(values_from = diagnosis, names_from = method) |>
	tidyr::drop_na(PCR) |>
	# This part makes the table
	dplyr::select(-unique_visit) |>
	dplyr::mutate(
		PCR = forcats::fct_relevel(PCR, "positive", "negative"),
		Clinician = forcats::fct_relevel(Clinician, "positive", "negative")
	) |>
	gtsummary::tbl_cross() |>
	gtsummary::as_flex_table() |>
	flextable::fontsize(size = 10)

readr::write_rds(
	cl_pcr_tab,
	here::here("Results", "Tables", "Cl-PCR.Rds")
)

# END OF FILE ####
