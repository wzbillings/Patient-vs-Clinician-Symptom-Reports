##############################################################################
# Poster figures and tables
# Zane Billings and Jacqueline Dworaczyk
# 2021-07-15
# Makes all figures and tables needed for the REU poster
##############################################################################

library(tidyverse)
library(here)
library(viridis)
library(broom)

#devtools::install_github("wz-billings/pROC")
library(pROC)

# Source packages
if (require('vcd')==FALSE) {install.packages('vcd', repos="https://cran.rstudio.com"); 
  require(vcd);}
if (require('tidyverse')==FALSE) {install.packages('tidyverse', repos="https://cran.rstudio.com"); 
  require(tidyverse);}
if (require('forcats')==FALSE) {install.packages('forcats', repos="https://cran.rstudio.com"); 
  require(forcats);}

# Source helper functions
source("4 Analysis Scripts/Kappa_Tables_Helper.R")

# -----------------------------------------------------------------------------
# Get Kappa statistic and confidence intervals for each symptom, for plotting
# -----------------------------------------------------------------------------
get_kappa_CI_table <- function(agreement_table_list) {
  # Names of all symptoms/signs that we are comparing 
  Symptoms<-c("Acute Onset", "Cough", "Fever", "Chills Sweats",  
              "Nasal Congestion", "Runny Nose", "Sore Throat", "Headache", 
              "Ear Pain", "Wheeze", "Chest Pain", "Breathless", 
              "Swollen Lymph Nodes", "Myalgia", "Chest Congestion",
              "Fatigue", "Sneeze", "Eye Irritation", "Tooth Pain", 
              "Eye Pain")
  
  # Calculate kappa for each symptom/sign
  kappas <- get_kappas(agreement_table_list)
  
  # Calculate upper CI for kappa statistic of each symptom
  kappas_upr <- get_kappa_uprCIs(agreement_table_list)
  
  # Calculate lower CI for kappa statistic of each symptom
  kappas_lwr <- get_kappa_lwrCIs(agreement_table_list)
  
  # Calculate agreements for each kappa
  agreement <- get_agreements(kappas)
  # factor agreements from low to high
  levels = c("None", "Minimal", "Weak", "Moderate")
  agreement_f <- factor(agreement, levels = levels, ordered = TRUE)
  
  
  # put together and convert to data frame
  Symptoms_f <- as.factor(Symptoms)
  KappaCI_df <- data.frame(Symptoms_f, kappas, kappas_lwr, kappas_upr, 
                           agreement, agreement_f)
  rownames(KappaCI_df) <- NULL
  
  return(KappaCI_df)
}


# -----------------------------------------------------------------------------
# Helper function that generates colored kappa confidence interval plot for 
# either the entire population, the male population or the female population
# -----------------------------------------------------------------------------
get_kappa_CI_plot_c <- function(kappa_CIs_df_ordered) {
  kappaCI_plot_c <- kappa_CIs_df_ordered %>%
    ggplot() + 
    geom_pointrange(mapping = aes(x = kappas, y = Symptom_f_lh,
                                  xmin = kappas_lwr, xmax = kappas_upr,
                                  color = agreement_f),
                    size = 1) +
    scale_color_manual(values = asuPalette) +
    coord_cartesian(xlim = c(0:1)) +
    labs(x ="Cohen's Kappa") +
    labs(color = "Agreement") +
    labs(y = "") +
    theme_bw() +
    theme(axis.text = element_text(color = "black", size = 14),
          axis.title.x = element_text(color = "black", size = 14)) +
    theme(legend.position = c(0.75, 0.25), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(kappaCI_plot_c)
}

dat <- readRDS("3 Clean Data/Symptoms.Rda")

# palette for use in plotting
asuPalette <- c("#8C1D40", "#FFC627", "#78BE20", "#00A3E0", "#FF7F32", 
                "#747474")

# Make 2x2 symptom agreement tables for each symptom evaluated by patient and 
# clinician
agreement_table_list <- get_symptom_agreement_tables(dat)

# Make kappa-CI dataframe (for plotting kappa CI graphs)
kappa_CIs_df <- get_kappa_CI_table(agreement_table_list)

# Order symptoms 
kappa_CIs_df_ordered <- kappa_CIs_df %>% 
  mutate(Symptom_f_lh = forcats::fct_reorder(Symptoms_f, kappas, max))

# Make colored kappaCI_plot for entire population
kappaCI_plot_c <- get_kappa_CI_plot_c(kappa_CIs_df_ordered)
print(kappaCI_plot_c)




roc_data <- readRDS(here::here("5 Results", "Analysis_Data", "ROC_Data.Rda"))

roc_plots <- roc_data %>%
  tidyr::unnest(roc_coords) %>%
  dplyr::filter(outcome == "Clinician Diagnosis")

pt_vs_clinician_rocs <- ggplot(roc_plots, aes(x = 1 - specificity, 
                                              y = sensitivity, col = score)) +
  geom_line(size = 1) +
  facet_wrap(~rater, labeller = label_value) +
  theme_bw(base_size = 16) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
  geom_abline( # add line of no discrimination
    intercept = 0,
    slope = 1,
    color = "darkgrey",
    linetype = "dashed"
  ) +
  scale_color_manual(values = asuPalette) +
  theme(axis.text = element_text(color = "black")) +
  theme(legend.justification = 0.5) +
  labs(color = "")
print(pt_vs_clinician_rocs)


# table for confidence intervals stuff
AUC_tables <- roc_data %>%
  dplyr::filter(outcome == "Clinician Diagnosis") %>%
  dplyr::select(-c(roc_coords, outcome)) %>%
  tidyr::pivot_wider(
    id_cols = score,
    names_from = rater,
    values_from = ROC
  ) %>%
  dplyr::mutate(
    delong_test = purrr::map2(
      .x = `Clinician`,
      .y = `Patient`,
      .f = ~broom::tidy(
        pROC::roc.test(
          roc1 = .x, roc2 = .y,
          method = "delong"
        )
      )
    )
  ) %>%
  dplyr::select(score, delong_test) %>%
  tidyr::unnest(delong_test) %>%
  dplyr::mutate(
    across(where(is.numeric), ~round(.x, 3))
  ) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(
    score = score,
    cl_auc = estimate1,
    pt_auc = estimate2,
    diff = estimate1 - estimate2,
    diff_ci_95 = paste0("(", conf.low, " - ", conf.high, ")")
  )

pcr_AUC_tables <- roc_data %>%
  dplyr::filter(outcome == "PCR") %>%
  dplyr::select(-c(roc_coords, outcome)) %>%
  tidyr::pivot_wider(
    id_cols = score,
    names_from = rater,
    values_from = ROC
  ) %>%
  dplyr::mutate(
    delong_test = purrr::map2(
      .x = `Clinician`,
      .y = `Patient`,
      .f = ~broom::tidy(
        pROC::roc.test(
          roc1 = .x, roc2 = .y,
          method = "delong"
        )
      )
    )
  ) %>%
  dplyr::select(score, delong_test) %>%
  tidyr::unnest(delong_test) %>%
  dplyr::mutate(
    across(where(is.numeric), ~round(.x, 3))
  ) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(
    score = score,
    cl_auc = estimate1,
    pt_auc = estimate2,
    diff = estimate1 - estimate2,
    diff_ci_95 = paste0("(", conf.low, " - ", conf.high, ")")
  )


saveRDS(AUC_tables, 
        here::here("8 Auxiliary/Jackies_REU_poster/Figures/CI_table.Rda"))
saveRDS(pcr_AUC_tables, 
        here::here("8 Auxiliary/Jackies_REU_poster/Figures/PCR_CI_table.Rda"))
ggsave(file = here::here("8 Auxiliary/Jackies_REU_poster/Figures/Diagnosis_ROCS.png"),
       pt_vs_clinician_rocs,
       width = 6, height = 4, dpi=320)
ggplot2::ggsave(file = "8 Auxiliary/Jackies_REU_poster/Figures/KappaPlot_c.png", 
                kappaCI_plot_c, 
                width = 7, height = 5, dpi=320)

# Make bar plot of metrics
# Need to import metrics data
metrics_data <- readRDS("5 Results/Analysis_Data/Model_Metrics.Rda")
metrics_data <- metrics_data %>% 
  dplyr::filter(.metric %in% c("mcc", "sens", "spec")) %>%
  mutate(.metric = factor(droplevels(as.factor(.metric)), 
                          levels = c("mcc","sens", "spec"),
                          labels = c("MCC", "Sensitivity", "Specificity")),
         score = factor(score,
                        levels = c("CF", "CFA", "CFM", "Ebell3", "Tree1"),
                        labels = c("CF", "CFA", "CFM", "Score", "Tree")),
         rater = factor(rater, 
                        levels = c("Cl", "Pt"),
                        labels = c("Clinician", "Patient")))
metrics_ggplot <- metrics_data %>%
  ggplot(aes(x = score, y = .estimate, fill = rater)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = asuPalette) +
  facet_wrap(~.metric, nrow = 1) +
  cowplot::theme_cowplot() +
  theme(axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 13, angle = -2, vjust = 0),
        axis.text.y = element_text(size = 13),
        legend.position = "bottom", legend.justification = "center",
        text = element_text(size = 18)) +
  labs(y = "") +
  labs(x = "Clinical Decision Rule") +
  labs(fill = "")

ggplot2::ggsave(file = "8 Auxiliary/Jackies_REU_poster/Figures/Metrics.png", 
                metrics_ggplot, 
                width = 8, height = 3.5, dpi=320)
