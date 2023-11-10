###
# Generating figures for manuscript
# author: Zane Billings
# date: 2021-08-23
###

library("ggplot2")
library(patchwork)

# ggplot 2 theme setting ######################################################
plot_palette <- c(
	scales::viridis_pal(begin = 0, end = 0.8, option = "plasma")(2),
	"red"
)

str_round <- function(.x, d = 2) {
	format(round(.x, digits = d), nsmall = d)
}

ggplot2::theme_set(ggplot2::theme_classic(
	base_size = 28,
))
ggplot2::theme_update(
	legend.position = "bottom",
	legend.justification = "center",
	text = ggplot2::element_text(color = "black"),
	axis.text = ggplot2::element_text(color = "black")
)

# Agreement between diagnostic methods ########################################
dx_agreement_df <- readRDS(here::here("Results", "Data",
																			"diagnosis_agreement_df.Rds"))

dx_agreement_kappa_plot <-
	dx_agreement_df |>
	dplyr::filter(statistic == "Cohen's kappa") |>
	dplyr::mutate(
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 3)),
		method = forcats::fct_reorder(method, estimate)
	) |>
	ggplot(aes(x = estimate, xmin = lower, xmax = upper,
						 y = method, color = agreement)) +
	geom_errorbar(size = 2, width = 0.5, show.legend = FALSE) +
	geom_point(size = 5, shape = 21, fill = "white", stroke = 2.5) +
	geom_point(size = 3, shape = 3, stroke = 2) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	scale_x_continuous(labels = scales::number, limits = c(0,1)) +
	labs(x = "Cohen's kappa (95% bootstrap CI)", y = NULL,
			 color = "qualitative agreement") +
	theme(legend.position = c(0.8, 0.3),
				legend.key.size = unit(1, "cm"),
				legend.title = element_text(size = 18),
				legend.text = element_text(size = 16),
				axis.text = element_text(size = 18),
				axis.title = element_text(size = 20))

ggplot2::ggsave(file = "Results/Figures/dx_agreement_kappa_plot.tiff", 
								dx_agreement_kappa_plot,
								width = 12, height = 7.4, dpi=320, compression = "lzw")

## Panel plot with other four measures for supplement ####
dx_agreement_panel_plot <-
	dx_agreement_df |>
	dplyr::filter(statistic != "Cohen's kappa") |>
	dplyr::mutate(
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 3)),
		method = forcats::fct_reorder(method, estimate)
	) |>
	ggplot(aes(x = estimate, xmin = lower, xmax = upper,
						 y = method)) +
	geom_errorbar(size = 0.75, width = 0.45, show.legend = FALSE) +
	geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
	geom_point(size = 1, shape = 3, stroke = 1) +
	facet_wrap(vars(statistic)) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	scale_x_continuous(labels = scales::number, limits = c(0,1),
										 expand = expansion(0.03, 0)) +
	labs(x = "statistic (95% bootstrap CI)", y = NULL,
			 color = "qualitative agreement") +
	theme_classic(base_size = 16) +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2)
	)

ggplot2::ggsave(
	file = here::here("Results/Figures/dx_agreement_panel_plot.tiff"), 
	dx_agreement_panel_plot, 
	width = 12, height = 10, dpi=320
)

# Bar plot for symptom prevalence in diagnostic groups ####
dat_symp_prev <- readRDS(here::here("Data", "Clean-Data", "Symptoms-Long.Rds")) |>
	dplyr::group_by(method, diagnosis, rater, symptom) |>
	dplyr::summarize(ggplot2::mean_cl_boot(present, B = 100), .groups = "drop")

p1 <- dat_symp_prev |>
	dplyr::filter(method == "PCR") |>
	ggplot(ggplot2::aes(x = y, xmin = ymin, xmax = ymax,
											y = forcats::fct_reorder(symptom, y),
											color = diagnosis)) +
	geom_errorbar(size = 1, width = 0.5, show.legend = FALSE,
								position = position_dodge(width = 0.75)) +
	geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5,
						 position = position_dodge(width = 0.75)) +
	geom_point(size = 2, shape = 3, stroke = 1,
						 position = position_dodge(width = 0.75)) +
	facet_grid(~rater) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	labs(y = NULL, x = "proportion of reports") +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2,
																			size = 0.5)
	)

p2 <- dat_symp_prev |>
	dplyr::filter(method == "PCR") |>
	ggplot(ggplot2::aes(x = y, xmin = ymin, xmax = ymax,
											y = forcats::fct_reorder(symptom, y),
											color = rater)) +
	geom_errorbar(size = 1, width = 0.5, show.legend = FALSE,
								position = position_dodge(width = 0.75)) +
	geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5,
						 position = position_dodge(width = 0.75)) +
	geom_point(size = 2, shape = 3, stroke = 1,
						 position = position_dodge(width = 0.75)) +
	facet_grid(~diagnosis) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	labs(y = NULL, x = "proportion of reports") +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2,
																			size = 0.5)
	)

p2b <- dat_symp_prev |>
	dplyr::filter(method == "PCR") |>
	ggplot(ggplot2::aes(x = y, xmin = ymin, xmax = ymax,
											y = forcats::fct_reorder(symptom, y),
											fill = rater)) +
	geom_col(position = 'dodge') +
	facet_grid(~diagnosis) +
	scale_fill_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	labs(y = NULL, x = "proportion of reports") +
	scale_x_continuous(expand = c(0,0, 0.02, 0), limits = c(0, 1)) +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2,
																			size = 0.5),
		panel.spacing.x = unit(1, "in")
	)

P3 <-
	readRDS(here::here("Data", "Clean-Data", "Symptoms-Long.Rds")) |>
	dplyr::group_by(method, diagnosis, symptom) |>
	dplyr::summarize(ggplot2::mean_cl_boot(present, B = 100)) |>
	dplyr::ungroup() |>
	dplyr::filter(method == "PCR") |>
	ggplot(ggplot2::aes(x = y, xmin = ymin, xmax = ymax,
											y = forcats::fct_reorder(symptom, y),
											color = Diagnosis)) +
	geom_errorbar(size = 1, width = 0.5, show.legend = FALSE,
								position = position_dodge(width = 0.75)) +
	geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5,
						 position = position_dodge(width = 0.75)) +
	geom_point(size = 2, shape = 3, stroke = 1,
						 position = position_dodge(width = 0.75)) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	labs(y = NULL, x = "proportion of reports") +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2,
																			size = 0.5)
	)

# Interrater reliability plots ################################################

### Kappas plot ####
irr_df <- readRDS(here::here("Results", "Data",
														 "symptoms_irr_df.Rds"))

# Add score labels to symptoms that are used in the scores
irr_df <-
	irr_df |>
	dplyr::mutate(
		symptom = as.character(symptom),
		symptom = 
			dplyr::case_when(
				symptom == "Acute Onset" ~ "Acute Onset (CFA, WS, TM)",
				symptom == "Cough" ~ "Cough (all CDRs)",
				symptom == "Subjective Fever" ~ "Subjective Fever (all CDRs)",
				symptom == "Myalgia" ~ "Myalgia (CFM, WS)",
				symptom == "Chills Sweats" ~ "Chills/Sweats (TM, WS)",
				TRUE ~ symptom
			),
		symptom = factor(symptom)
	)

lines_df <-
	tibble::tibble(
		max = c(0.2, 0.39, 0.59, 0.79, 0.90, 1.00),
		min = c(0.0, 0.20, 0.39, 0.59, 0.79, 0.90),
		agreement = forcats::fct_inorder(
			c("None", "Minimal", "Weak", "Moderate", "High", "Almost Perfect")
		)
	)

pcr_kappa_plot <-
	irr_df |>
	dplyr::filter(statistic == "Cohen's kappa",
								method == "pcr") |>
	dplyr::mutate(
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 3)),
		symptom = forcats::fct_reorder(symptom, estimate),
		agreement = forcats::fct_drop(agreement, only = c("Negative"))
	) |>
	ggplot2::ggplot(ggplot2::aes(x = estimate, xmin = lower, xmax = upper,
															 y = symptom, color = agreement)) +
	ggplot2::geom_rect(
		data = lines_df,
		mapping = ggplot2::aes(
			xmin = min, xmax = max,
			ymin = -Inf, ymax = Inf,
			fill = agreement
		),
		alpha = 0.25,
		inherit.aes = FALSE,
		#show.legend = FALSE
	) +
	ggplot2::geom_errorbar(size = 2, width = 0.5, show.legend = FALSE) +
	ggplot2::geom_point(
		size = 5, stroke = 2.5, fill = NA, shape = 21
	) +
	ggplot2::geom_point(size = 3, shape = 3, stroke = 2) +
	ggplot2::scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8,
																 direction = 1, drop = F) +
	ggplot2::scale_fill_viridis_d(option = "plasma", begin = 0, end = 0.8,
																direction = 1, drop = F) +
	ggplot2::scale_x_continuous(labels = scales::number) +
	ggplot2::coord_cartesian(xlim = c(0, 1)) +
	ggplot2::labs(
		x = "Cohen's kappa (95% bootstrap CI)",
		y = NULL,
		color = "qualitative agreement",
		shape = "qualitative agreement",
		fill = "qualitative agreement"
	) +
	ggplot2::theme(
		legend.key.size = ggplot2::unit(1, "cm"),
		legend.title = ggplot2::element_text(size = 18),
		legend.text = ggplot2::element_text(size = 16),
		axis.text = ggplot2::element_text(size = 18),
		axis.title = ggplot2::element_text(size = 20)
	); pcr_kappa_plot

ggplot2::ggsave(file = "Results/Figures/pcr_kappa_plot.tiff", 
								pcr_kappa_plot, 
								width = 12, height = 7.4, dpi=320, compression = "lzw")

### Panel plot with other four measures for supplement ####
pcr_panel_plot <-
	irr_df |>
	dplyr::filter(statistic != "Cohen's kappa",
								method == "pcr") |>
	dplyr::mutate(
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 3)),
		symptom = forcats::fct_reorder(symptom, estimate)
	) |>
	ggplot(aes(x = estimate, xmin = lower, xmax = upper,
						 y = symptom)) +
	geom_errorbar(size = 0.75, width = 0.45, show.legend = FALSE) +
	geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
	geom_point(size = 1, shape = 3, stroke = 1) +
	facet_wrap(vars(statistic)) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8,
												drop = FALSE) +
	scale_x_continuous(labels = scales::number, limits = c(-0.17,1),
										 expand = expansion(0.03, 0)) +
	labs(x = "statistic (95% bootstrap CI)", y = NULL,
			 color = "qualitative agreement") +
	theme_classic(base_size = 16) +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2)
	); pcr_panel_plot

ggplot2::ggsave(file = here::here("Results/Figures/pcr_irr_panel_plot.tiff"), 
								pcr_panel_plot, 
								width = 12, height = 10, dpi=320)

# Score agreement plots ####

## Discrete (heuristic) score agreement plots ####
heur_dat <-
	readRDS(here::here("Results/Data/heurisics_agreement.Rds"))

# Dot/whisker plot of kappas
pcr_heur_kappas <-
	heur_dat |>
	dplyr::filter(method == "pcr") |>
	dplyr::filter(statistic == "Cohen's kappa") |>
	dplyr::mutate(
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 3)),
		CPR = forcats::fct_rev(CPR),
		agreement = factor(
			agreement,
			levels = c("None", "Minimal", "Weak", "Moderate", "High", "Almost Perfect")
		)
	) |>
	ggplot(aes(x = estimate, xmin = lower, xmax = upper,
						 y = CPR, color = agreement)) +
	ggplot2::geom_rect(
		data = lines_df,
		mapping = ggplot2::aes(
			xmin = min, xmax = max,
			ymin = -Inf, ymax = Inf,
			fill = agreement
		),
		alpha = 0.25,
		inherit.aes = FALSE,
		#show.legend = FALSE
	) +
	geom_errorbar(size = 2, width = 0.5, show.legend = FALSE) +
	geom_point(size = 5, shape = 21, fill = NA, stroke = 2.5) +
	geom_point(size = 3, shape = 3, stroke = 2) +
	ggplot2::scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8,
																 direction = 1, drop = F) +
	ggplot2::scale_fill_viridis_d(option = "plasma", begin = 0, end = 0.8,
																direction = 1, drop = F) +
	scale_x_continuous(labels = scales::number, limits = c(0,1)) +
	labs(
		x = "Cohen's kappa (95% bootstrap CI)",
		y = NULL,
		color = "qualitative agreement",
		fill = "qualitative agreement"
	) +
	theme(
		
				legend.key.size = unit(1, "cm"),
				legend.title = element_text(size = 18),
				legend.text = element_text(size = 16),
				axis.text = element_text(size = 18),
				axis.title = element_text(size = 20)
				)

ggplot2::ggsave(file = "Results/Figures/pcr_heur_kappa_plot.tiff", 
								pcr_heur_kappas, 
								width = 12, height = 7.4, dpi=320, compression = "lzw")

pcr_heur_irr_plot <-
	heur_dat |>
	dplyr::filter(method == "pcr") |>
	dplyr::filter(statistic != "Cohen's kappa") |>
	dplyr::mutate(
		dplyr::across(tidyselect:::where(is.numeric), ~round(.x, 3)),
		CPR = forcats::fct_rev(CPR)
	) |>
	ggplot(aes(x = estimate, xmin = lower, xmax = upper,
						 y = CPR)) +
	geom_errorbar(size = 0.75, width = 0.45, show.legend = FALSE) +
	geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
	geom_point(size = 1, shape = 3, stroke = 1) +
	facet_wrap(vars(statistic)) +
	scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
	scale_x_continuous(labels = scales::number, limits = c(0,1),
										 expand = expansion(0.03, 0)) +
	labs(x = "statistic (95% bootstrap CI)", y = NULL,
			 color = "qualitative agreement") +
	theme_classic(base_size = 16) +
	theme(
		legend.position = "bottom",
		axis.text.y = element_text(color = "black"),
		axis.text.x = element_text(color = "black"),
		panel.grid.major.y = element_line(color = "lightgray", linetype = 2)
	)

ggplot2::ggsave(file = here::here("Results/Figures/pcr_heur_panel_plot.tiff"), 
								pcr_heur_irr_plot, 
								width = 12, height = 10, dpi=320)

## Plots for continuous CDRs (WS, TM) ####
ba_dat <- readRDS(here::here("Results/Data/long_score_dat.Rds")) |>
	dplyr::filter(CPR %in% c("Score", "Tree")) |>
	dplyr::filter(method == "pcr")

### WS rule ####
ba_limits_ws <-
	ba_dat |>
	dplyr::filter(CPR == "Score") |>
	dplyr::group_by(CPR, method) |>
	dplyr::mutate(m = cl - pt) |>
	dplyr::summarise(
		mean_score = mean(m),
		sd_score = sd(m),
		.groups = "drop"
	) |>
	dplyr::mutate(
		lwr = mean_score - 1.96 * sd_score,
		upr = mean_score + 1.96 * sd_score
	)

# Simple correlation plot
ws_corr_plot <-
	ba_dat |>
	dplyr::filter(CPR == "Score") |>
	ggplot2::ggplot(
		mapping = ggplot2::aes(
			x = pt,
			y = cl
		)
	) +
	# Reference line for perfect agreement
	ggplot2::geom_abline(
		slope = 1,
		intercept = 0,
		linetype = 2,
		color = "black"
	) +
	ggplot2::geom_count(
		mapping = ggplot2::aes(fill = ggplot2::after_stat(n)),
		shape = 21
	) +
	ggplot2::scale_fill_viridis_b(
		breaks = c(1, 5, 10, 15, 30, 46),
		option = "plasma", begin = 0, end = 1
	) +
	ggplot2::scale_size(
		breaks = c(1, 5, 10, 15, 30, 46),
		range = c(2, 10)
	) +
	ggplot2::guides(
		fill = ggplot2::guide_legend(title = "count", byrow = TRUE),
		size = ggplot2::guide_legend(title = "count", byrow = TRUE)
	) +
	ggplot2::labs(
		x = "Patient score",
		y = "Clinician score"
	) +
	ggplot2::coord_equal(xlim = c(0, 6), ylim = c(0, 6))

# Bland-Altman plot
ws_ba_plot <-
	ggplot2::ggplot(
		data = ba_dat |> dplyr::filter(CPR == "Score"),
		mapping = ggplot2::aes(
			x = (cl + pt) / 2,
			y =  cl - pt
		)
	) +
	# Mean line
	ggplot2::geom_hline(
		data = ba_limits_ws,
		mapping = ggplot2::aes(
			yintercept = mean_score
		),
		color = "red"
	) +
	# Lower limit
	ggplot2::geom_hline(
		data = ba_limits_ws,
		mapping = ggplot2::aes(
			yintercept = lwr
		),
		linetype = 2
	) +
	# Upper limit
	ggplot2::geom_hline(
		data = ba_limits_ws,
		mapping = ggplot2::aes(
			yintercept = upr
		),
		linetype = 2
	) +
	ggplot2::geom_count(
		mapping = ggplot2::aes(fill = ggplot2::after_stat(n)),
		shape = 21
	) +
	ggplot2::scale_fill_viridis_b(
		breaks = c(1, 5, 10, 15, 30, 46),
		option = "plasma", begin = 0, end = 1
	) +
	ggplot2::scale_size(
		breaks = c(1, 5, 10, 15, 30, 46),
		range = c(2, 10)
	) +
	ggplot2::guides(
		fill = ggplot2::guide_legend(title = "count", byrow = TRUE),
		size = ggplot2::guide_legend(title = "count", byrow = TRUE)
	) +
	ggplot2::labs(
		x = "Average",
		y = "Difference\n(clinician - patient)"
	) +
	ggplot2::coord_equal(xlim = c(0, 6), ylim = c(-6, 6), ratio = 0.5)

# Panel plot
ws_panel <-
	ws_corr_plot +
	ws_ba_plot +
	patchwork::plot_layout(guides = "collect") &
	ggplot2::theme(axis.title = ggplot2::element_text(size = 24))

# Saving plots to disk
ggplot2::ggsave(file = here::here("Results/Figures/ws_ba.tiff"), 
								ws_ba_plot, 
								width = 12, height = 10, dpi=320)

ggplot2::ggsave(file = here::here("Results/Figures/ws_corr.tiff"), 
								ws_corr_plot, 
								width = 12, height = 10, dpi=320)

ggplot2::ggsave(file = here::here("Results/Figures/ws_panel.tiff"), 
								ws_panel, 
								width = 12, height = 10, dpi=320)

### TM rule ####
ba_limits_tm <-
	ba_dat |>
	dplyr::filter(CPR == "Tree") |>
	dplyr::group_by(CPR, method) |>
	dplyr::mutate(m = cl - pt) |>
	dplyr::summarise(
		mean_score = mean(m),
		sd_score = sd(m),
		.groups = "drop"
	) |>
	dplyr::mutate(
		lwr = mean_score - 1.96 * sd_score,
		upr = mean_score + 1.96 * sd_score
	)

# Simple correlation plot
tm_corr_plot <-
	ba_dat |>
	dplyr::filter(CPR == "Tree") |>
	ggplot2::ggplot(
		mapping = ggplot2::aes(
			x = pt,
			y = cl
		)
	) +
	# Reference line for perfect agreement
	ggplot2::geom_abline(
		slope = 1,
		intercept = 0,
		linetype = 2,
		color = "black"
	) +
	ggplot2::geom_count(
		mapping = ggplot2::aes(fill = ggplot2::after_stat(n)),
		shape = 21
	) +
	ggplot2::scale_fill_viridis_b(
		breaks = c(5, 10, 15, 30, 45, 60),
		option = "plasma", begin = 0, end = 1
	) +
	ggplot2::scale_size(
		breaks = c(5, 10, 15, 30, 45, 60),
		range = c(2, 10)
	) +
	ggplot2::guides(
		fill = ggplot2::guide_legend(title = "count", byrow = TRUE),
		size = ggplot2::guide_legend(title = "count", byrow = TRUE)
	) +
	ggplot2::labs(
		x = "Patient score",
		y = "Clinician score"
	) +
	ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1))

# Bland-Altman plot
tm_ba_plot <-
	ggplot2::ggplot(
		data = ba_dat |> dplyr::filter(CPR == "Tree"),
		mapping = ggplot2::aes(
			x = (cl + pt) / 2,
			y =  cl - pt
		)
	) +
	# Mean line
	ggplot2::geom_hline(
		data = ba_limits_tm,
		mapping = ggplot2::aes(
			yintercept = mean_score
		),
		color = "red"
	) +
	# Lower limit
	ggplot2::geom_hline(
		data = ba_limits_tm,
		mapping = ggplot2::aes(
			yintercept = lwr
		),
		linetype = 2
	) +
	# Upper limit
	ggplot2::geom_hline(
		data = ba_limits_tm,
		mapping = ggplot2::aes(
			yintercept = upr
		),
		linetype = 2
	) +
	ggplot2::geom_count(
		mapping = ggplot2::aes(fill = ggplot2::after_stat(n)),
		shape = 21
	) +
	ggplot2::scale_fill_viridis_b(
		breaks = c(5, 10, 15, 30, 45, 60),
		option = "plasma", begin = 0, end = 1
	) +
	ggplot2::scale_size(
		breaks = c(5, 10, 15, 30, 45, 60),
		range = c(2, 10)
	) +
	ggplot2::guides(
		fill = ggplot2::guide_legend(title = "count", byrow = TRUE),
		size = ggplot2::guide_legend(title = "count", byrow = TRUE)
	) +
	ggplot2::labs(
		x = "Average",
		y = "Difference\n(clinician - patient)"
	) +
	ggplot2::coord_equal(xlim = c(0, 1), ylim = c(-1, 1), ratio = 0.5)

# Panel plot
tm_panel <-
	tm_corr_plot +
	tm_ba_plot +
	patchwork::plot_layout(guides = "collect") &
	ggplot2::theme(axis.title = ggplot2::element_text(size = 24))

# Saving plots to disk
ggplot2::ggsave(file = here::here("Results/Figures/tm_ba.tiff"), 
								tm_ba_plot, 
								width = 12, height = 10, dpi=320)

ggplot2::ggsave(file = here::here("Results/Figures/tm_corr.tiff"), 
								tm_corr_plot, 
								width = 12, height = 10, dpi=320)

ggplot2::ggsave(file = here::here("Results/Figures/tm_panel.tiff"), 
								tm_panel, 
								width = 12, height = 10, dpi=320)

## Paneling both continuous scores together ####

sa_panel <-
	(ws_corr_plot + ggtitle("Weighted score model (WS)")) +
	(tm_corr_plot + ggtitle("Tree model (TM)")) &
	ggplot2::theme(
		plot.title = ggplot2::element_text(hjust = 0.5, size = 26),
		axis.title = ggplot2::element_text(size = 24)
	)

ggplot2::ggsave(
	file = here::here("Results/Figures/score_agreement_panel.tiff"), 
	sa_panel, 
	width = 12, height = 10, dpi=320
)

# Plots of number of symptoms reported ####

# Calculation of number of symptoms (probably redudant but I didn't
# want to rewrite this code since it works)
symptom_counts <-
	readRDS(here::here("Data", "Clean-Data", "Symptoms-Long.Rds")) |>
	dplyr::group_by(method, rater, unique_visit) |>
	dplyr::summarize(count = sum(present), .groups = "drop") |>
	tidyr::pivot_wider(
		names_from = "rater",
		values_from = count
	) |>
	dplyr::filter(method == "PCR") |>
	dplyr::select(-method)

# Calculation of BA plot lines
count_limits <-
	symptom_counts |>
	dplyr::mutate(m = clinician - patient) |>
	dplyr::summarise(
		mean_score = mean(m),
		sd_score = sd(m),
		.groups = "drop"
	) |>
	dplyr::mutate(
		lwr = mean_score - 1.96 * sd_score,
		upr = mean_score + 1.96 * sd_score
	)

# Bland-Altman plot
symp_count_ba <-
	ggplot2::ggplot(
		data = symptom_counts,
		mapping = ggplot2::aes(
			x = (clinician + patient) / 2,
			y =  clinician - patient
		)
	) +
	# Mean line
	ggplot2::geom_hline(
		data = count_limits,
		mapping = ggplot2::aes(
			yintercept = mean_score
		),
		color = "red"
	) +
	# Lower limit
	ggplot2::geom_hline(
		data = count_limits,
		mapping = ggplot2::aes(
			yintercept = lwr
		),
		linetype = 2
	) +
	# Upper limit
	ggplot2::geom_hline(
		data = count_limits,
		mapping = ggplot2::aes(
			yintercept = upr
		),
		linetype = 2
	) +
	ggplot2::geom_count(
		mapping = ggplot2::aes(fill = ggplot2::after_stat(n)),
		shape = 21
	) +
	ggplot2::scale_fill_viridis_b(
		breaks = c(1, 3, 6, 9, 12, 15),
		option = "plasma", begin = 0, end = 1
	) +
	ggplot2::scale_size(
		breaks = c(1, 3, 6, 9, 12, 15),
		range = c(2, 10)
	) +
	ggplot2::guides(
		fill = ggplot2::guide_legend(title = "count"),
		size = ggplot2::guide_legend(title = "count")
	) +
	ggplot2::labs(
		x = "Average",
		y = "Difference (clinician - patient)"
	) +
	ggplot2::coord_equal(xlim = c(0, 20), ylim = c(-11, 11))

# Simple correlation plot
symp_count_corr <-
	ggplot2::ggplot(
		data = symptom_counts,
		mapping = ggplot2::aes(
			x = patient,
			y = clinician
		)
	) +
	# Refrence line for perfect agreement
	ggplot2::geom_abline(
		slope = 1,
		intercept = 0,
		linetype = 2,
		color = "black"
	) +
	ggplot2::geom_count(
		mapping = ggplot2::aes(fill = ggplot2::after_stat(n)),
		shape = 21
	) +
	ggplot2::scale_fill_viridis_b(
		breaks = c(1, 3, 6, 9, 12, 15),
		option = "plasma", begin = 0, end = 1
	) +
	ggplot2::scale_size(
		breaks = c(1, 3, 6, 9, 12, 15),
		range = c(2, 10)
	) +
	ggplot2::guides(
		fill = ggplot2::guide_legend(title = "count"),
		size = ggplot2::guide_legend(title = "count")
	) +
	ggplot2::labs(
		x = "Number of symptoms reported\nby patient",
		y = "Number of symptoms reported\nby clinician"
	) +
	ggplot2::coord_equal(xlim = c(0, 20), ylim = c(0, 20))

# Panel Plot
symp_count_panel <-
	symp_count_corr +
	symp_count_ba +
	patchwork::plot_layout(guides = "collect") &
	ggplot2::theme(axis.title = ggplot2::element_text(size = 24))

# Saving plots to disk
ggplot2::ggsave(file = here::here("Results/Figures/symp_count_ba.tiff"), 
								symp_count_ba, 
								width = 12, height = 10, dpi=320)

ggplot2::ggsave(file = here::here("Results/Figures/symp_count_corr.tiff"), 
								symp_count_corr, 
								width = 12, height = 10, dpi=320)

ggplot2::ggsave(file = here::here("Results/Figures/symp_count_panel.tiff"), 
								symp_count_panel, 
								width = 12, height = 10, dpi=320)

# Plotting the ctrees ####

# THe conditional inference trees were selected as the best, so here
# we will plot them.

require(ggparty)

## Patient data ####

pt_trees <-
	readr::read_rds(
		here::here("Results", "Data", "Pt", "Tree-Models",
							 "Tree-Models.Rds")
	)
pt_ctree <- pt_trees[[4]]
	
ctree_plot_pt <-
	ggparty(pt_ctree) +
	geom_edge() +
	geom_edge_label() +
	geom_node_splitvar() +
	# pass list to gglist containing all ggplot components we want to plot for each
	# (default: terminal) node
	geom_node_plot(
		gglist = list(
			geom_bar(
				aes(x = "", fill = diagnosis),
				position = position_dodge()
			),
			geom_text(
				aes(x = "", group = diagnosis, label = after_stat(count)),
				position = position_dodge(width = 0.95),
				stat = "count",
				vjust = -0.1,
				size = 3.25,
				fontface = "bold",
			),
			xlab(NULL),
			ylab(NULL),
			scale_y_continuous(expand = c(0, 0), labels = NULL),
			coord_cartesian(ylim = c(0, 65)),
			zlib::theme_ms(),
			theme(
				legend.title = element_text(size = 12),
				legend.text = element_text(size = 10),
				plot.background = element_rect(fill = "white", color = "white")
			),
			scale_fill_manual(values = c("#E69F00", "#56B4E9"))
		)
	) +
	theme(
		plot.background = element_rect(fill = "white", color = "white")
	)

ggsave(
	plot = ctree_plot_pt,
	filename = here::here("Results", "Figures", "ctree-pt.tiff"),
	width = 6,
	height = 4,
	dpi = 320
)

## Patient data ####

cl_trees <-
	readr::read_rds(
		here::here("Results", "Data", "Cl", "Tree-Models",
							 "Tree-Models.Rds")
	)
cl_ctree <- cl_trees[[4]]

ctree_plot_cl <-
	ggparty(cl_ctree) +
	geom_edge() +
	geom_edge_label() +
	geom_node_splitvar() +
	# pass list to gglist containing all ggplot components we want to plot for each
	# (default: terminal) node
	geom_node_plot(
		gglist = list(
			geom_bar(
				aes(x = "", fill = diagnosis),
				position = position_dodge()
			),
			geom_text(
				aes(x = "", group = diagnosis, label = after_stat(count)),
				position = position_dodge(width = 0.95),
				stat = "count",
				vjust = -0.1,
				size = 3.25,
				fontface = "bold",
			),
			xlab(NULL),
			ylab(NULL),
			scale_y_continuous(expand = c(0, 0), labels = NULL),
			coord_cartesian(ylim = c(0, 65)),
			zlib::theme_ms(),
			theme(
				legend.title = element_text(size = 12),
				legend.text = element_text(size = 10),
				plot.background = element_rect(fill = "white", color = "white")
			),
			scale_fill_manual(values = c("#E69F00", "#56B4E9"))
		)
	) +
	theme(
		plot.background = element_rect(fill = "white", color = "white")
	)

ggsave(
	plot = ctree_plot_cl,
	filename = here::here("Results", "Figures", "ctree-cl.tiff"),
	width = 6,
	height = 4,
	dpi = 320
)

# END OF FILE ####

