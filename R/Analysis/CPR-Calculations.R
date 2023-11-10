##############################################################################
# Score computations
# Zane Billings, Jacqueline Dworaczyk
# 2021-07-13
# This script will calculate all of the flu scores we are in and add them to
#   the data frame of symptom and result information. 
# Calculates:
#   CF_Score: cough and fever decision rule (citation).
#     TRUE if both cough and fever are present. FALSE otherwise.
#   CFA_Score: cough, fever, and acute_onset onset decision rule (citation).
#     TRUE if both cough and fever are present and the patient has been ill
#     for <= 2 days. FALSE otherwise.
#   CFM_Score: cough, fever, and myalgia decision rule (citation).
#     TRUE if cough, fever, and myalgia are all present. FALSE otherwise.
#   Ebell3_Score: computes a score from 0 to 6 based on symptoms (citation).
#     The score is based on acute_onset onset of illness (1 pt), presence of both
#     cough and fever simultaneously (2 pt), presence of myalgia (2 pt), and
#     presence of chills/sweats (1 pt).
#   Tree1_Score: uses the first decision tree from Afonso 2012.
#     One caveat is that this tree uses the numerical fever, which is not
#     available in our data. So, we assume that all reported cases of
#     subjective_fever represent a temperature >= 37.89 degrees C, and all cases
#     where subjective_fever is absent represent a temperature < 37.4 degrees C.
#   CF_Pred, CFA_Pred, CFM_Pred: 0/1 versions of the associated scores, renamed
#     for ease of filtering later on.
#   Ebell3_Pred: assigns the Ebell score to a binary prediction.
#     In Ebell 2012, patients with flu score of 0 - 2 are assigned to low
#     risk, patients with flu score of 3 are assigned to medium risk, and
#     patients with flu score of 4 - 6 are assigned to high risk. Since the
#     likelihood for medium risk was reported as < 1, we assign a binary cutoff
#     of 0 for scores 0 - 3 and 1 for scores 4 - 6. Additionally,  the numerical
#     risk prediction for patients with a score of 4 - 6 was 59% while the risk
#     for patients with score < 4 was < 50%.
#   Tree1YN_Pred: assigns the Tree score to a binary prediction.
#     The tree estimates the risk of influenza for patients who are sorted into
#     a specific leaf of the tree. We assign a binary score of 1 for patients
#     with risk > 50%, 0 otherwise. 
##############################################################################

# Import dplyr pipe operator
library(dplyr, include.only = "%>%")

# Read in the clean data
dat_orig <- readRDS(here::here("Data", "Clean-Data", "Symptoms.Rds"))
dat <- dplyr::distinct(dat_orig)

# Calculate different flu diagnosis rules/scores results using the patient
#  reported symptom data.
pt_scores <- dat %>%
	dplyr::select(
		unique_visit,
		starts_with("pt_")
	) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(
		# Calculate actual scores
		CF_Score = pt_cough & pt_subjective_fever,
		CFA_Score = pt_cough & pt_subjective_fever & pt_acute_onset,
		CFM_Score = pt_cough & pt_subjective_fever & pt_myalgia,
		Ebell3_Score = (2 * (pt_cough & pt_subjective_fever) +
											2 * pt_myalgia + pt_acute_onset + pt_chills_sweats),
		Tree1_Score = dplyr::case_when(
			pt_subjective_fever & pt_acute_onset & pt_cough ~ 0.7833,
			pt_subjective_fever & pt_acute_onset & !pt_cough ~ 0.2500,
			pt_subjective_fever & !pt_acute_onset & pt_myalgia ~ 0.4627,
			pt_subjective_fever & !pt_acute_onset & !pt_myalgia ~ 0.1429,
			!pt_subjective_fever & pt_chills_sweats ~ 0.1809,
			!pt_subjective_fever & !pt_chills_sweats ~ 0.0571,
			TRUE ~ NA_real_
		),
		Tree2_Score = dplyr::case_when(
			pt_subjective_fever ~ 0.5443,
			!pt_subjective_fever & pt_chills_sweats ~ 0.1828,
			!pt_subjective_fever & pt_chills_sweats ~ 0.0563
		)
	) %>%
	dplyr::mutate(
		# Calculate binary predictions based on scores
		CF_Pred     = factor(CF_Score,  c(FALSE, TRUE), labels = c("low", "high")),
		CFA_Pred    = factor(CFA_Score, c(FALSE, TRUE), labels = c("low", "high")),
		CFM_Pred    = factor(CFM_Score, c(FALSE, TRUE), labels = c("low", "high")),
		# Versions with low/medium/high risk group
		Ebell3_Pred = dplyr::case_when(
			Ebell3_Score  < 3 ~ "low",
			Ebell3_Score == 3 ~ "med",
			Ebell3_Score  > 3 ~ "high",
			TRUE ~ NA_character_
		),
		Tree1_Pred = dplyr::case_when(
			Tree1_Score <= 0.10 ~ "low",
			Tree1_Score <= 0.50 ~ "med",
			Tree1_Score <= 1.00 ~ "high",
			TRUE ~ NA_character_
		)
	) %>%
	dplyr::select(!starts_with("pt")) %>%
	dplyr::ungroup() %>%
	dplyr::rename_with(
		.cols = !unique_visit,
		.fn = ~paste0("pt_", .x)
	)

# Calculate different flu diagnosis rules/scores results using the clinician
#  reported symptom data. Since there are only two cases we decided to copy
#  and paste the above code and change for clinician data, but if any more
#  cases are added, this should be refactored.
cl_scores <- dat %>%
	dplyr::select(
		unique_visit,
		starts_with("cl_")
	) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(
		CF_Score = cl_cough & cl_subjective_fever,
		CFA_Score = cl_cough & cl_subjective_fever & cl_acute_onset,
		CFM_Score = cl_cough & cl_subjective_fever & cl_myalgia,
		Ebell3_Score = (2 * (cl_cough & cl_subjective_fever) +
											2 * cl_myalgia + cl_acute_onset + cl_chills_sweats),
		Tree1_Score = dplyr::case_when(
			cl_subjective_fever & cl_acute_onset & cl_cough ~ 0.7833,
			cl_subjective_fever & cl_acute_onset & !cl_cough ~ 0.2500,
			cl_subjective_fever & !cl_acute_onset & cl_myalgia ~ 0.4627,
			cl_subjective_fever & !cl_acute_onset & !cl_myalgia ~ 0.1429,
			!cl_subjective_fever & cl_chills_sweats ~ 0.1809,
			!cl_subjective_fever & !cl_chills_sweats ~ 0.0571,
			TRUE ~ NA_real_
		)
	) %>%
	dplyr::mutate(
		# Calculate binary predictions based on scores
		CF_Pred     = factor(CF_Score,  c(FALSE, TRUE), labels = c("low", "high")),
		CFA_Pred    = factor(CFA_Score, c(FALSE, TRUE), labels = c("low", "high")),
		CFM_Pred    = factor(CFM_Score, c(FALSE, TRUE), labels = c("low", "high")),
		# Versions with low/medium/high risk group
		Ebell3_Pred = dplyr::case_when(
			Ebell3_Score  < 3 ~ "low",
			Ebell3_Score == 3 ~ "med",
			Ebell3_Score  > 3 ~ "high",
			TRUE ~ NA_character_
		),
		Tree1_Pred = dplyr::case_when(
			Tree1_Score <= 0.10 ~ "low",
			Tree1_Score <= 0.50 ~ "med",
			Tree1_Score <= 1.00 ~ "high",
			TRUE ~ NA_character_
		)
	) %>%
	dplyr::select(!starts_with("cl")) %>%
	dplyr::ungroup() %>%
	dplyr::rename_with(
		.cols = !unique_visit,
		.fn = ~paste0("cl_", .x)
	)

dat2 <- dplyr::inner_join(
	dat,
	pt_scores,
	by = "unique_visit"
)

dat3 <- dplyr::inner_join(
	dat2,
	cl_scores,
	by = "unique_visit"
)

saveRDS(dat3, here::here("Results", "Data", "Score-Data.Rds"))
