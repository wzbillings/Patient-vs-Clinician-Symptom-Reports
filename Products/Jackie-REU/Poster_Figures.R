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

devtools::install_github("wz-billings/pROC")
library(pROC)

roc_data <- readRDS(here::here("5 Results", "Analysis_Data", "ROC_Data.Rda"))

roc_plots <- roc_data %>%
  tidyr::unnest(roc_coords) %>%
  dplyr::filter(outcome == "Clinician Diagnosis")

ggplot(roc_plots, aes(x = 1 - specificity, y = sensitivity, col = score)) +
  geom_line(size = 1) +
  facet_wrap(~rater, labeller = label_both) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") +
  geom_abline( # add line of no discrimination
    intercept = 0,
    slope = 1,
    color = "darkgrey",
    linetype = "dashed"
  ) +
  scale_color_manual(values = c("deepskyblue", "firebrick3", "hotpink", "green", "darkorchid")) +
  theme(axis.text = element_text(color = "black"))



# table for confidence intervals stuff
test <- roc_data %>%
  dplyr::filter(outcome == "Clinician Diagnosis") %>%
  dplyr::select(-c(roc_coords, outcome)) %>%
  tidyr::pivot_wider(
    id_cols = score,
    names_from = rater,
    values_from = ROC
  ) %>%
  dplyr::mutate(
    delong_test = purrr::map2(
      .x = `Clinician-reported`,
      .y = `Patient-reported`,
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

saveRDS(test, here::here("5 Results/Analysis_Data/CI_table.Rda"))
