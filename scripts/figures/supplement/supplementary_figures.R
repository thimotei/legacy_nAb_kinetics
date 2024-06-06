library(ggridges)
library(ggnewscale)

# NOTE, some supplementary figures require the "id" variable, not just
# stan_id. Work is ongoing to ensure all figures that are possible to build
# without id run.

###############################
## FIGURE S1 - EVENT TIMINGS ##
###############################

#--- Vaccine timings

# Munging vaccine timings into right format for plotting all individual
dt_vax_dates_wide <- dt_all_data_plot[
  , .(dose_1_date, dose_2_date, dose_3_date, dose_4_date, dose_5_date),
  by = id]

dt_vax_types_wide <- dt_all_data_plot[
  , .(dose_1_type, dose_2_type, dose_3_type, dose_4_type, dose_5_type),
  by = id]

# Melting types and dates separately and merging (is there a way to do
# this in one go?)
dt_vax_dates <- melt(
  dt_vax_dates_wide,
  measure.vars = patterns("^dose_\\d+_date$"),
  value.name = "vax_date",
  variable.name = "dose") |> unique()

dt_vax_types <- melt(
  dt_vax_types_wide,
  measure.vars = patterns("^dose_\\d+_type$"),
  value.name = "vax_type",
  variable.name = "dose") |> unique()

# Removing date and types, just need which dose it is
dt_vax_dates[dose == "dose_1_date", dose := "1"]
dt_vax_dates[dose == "dose_2_date", dose := "2"]
dt_vax_dates[dose == "dose_3_date", dose := "3"]
dt_vax_dates[dose == "dose_4_date", dose := "4"]
dt_vax_dates[dose == "dose_5_date", dose := "5"]

dt_vax_types[dose == "dose_1_type", dose := "1"]
dt_vax_types[dose == "dose_2_type", dose := "2"]
dt_vax_types[dose == "dose_3_type", dose := "3"]
dt_vax_types[dose == "dose_4_type", dose := "4"]
dt_vax_types[dose == "dose_5_type", dose := "5"]

# Merging types and dates
dt_vax_long <- merge(
  dt_vax_dates, dt_vax_types,
  by = c("id", "dose"))

# Changing from factor to numeric, need to condition on dose
dt_vax_long[, dose := factor(dose, levels = c(1, 2, 3, 4, 5))][
  , dose := as.numeric(dose)]
dt_vax_long[, vax_type := factor(vax_type)]

# Removing NAs
dt_vax_long_trim <- dt_vax_long[
  , .(vax_date = unique(vax_date)),
  by = .(id, dose, vax_type)][
  order(id, dose)][!is.na(vax_date)]

# Add fake final date to create the line for the final dose per ID easily
dt_vax_long_trim[, max_dose := max(dose), by = id]
max_date <- dt_vax_long_trim[, max(vax_date)]
max_dose_types <- dt_vax_long_trim[dose == max_dose, .(id, max_dose, vax_type)]
extra_rows <- max_dose_types[, .(dose = max_dose, vax_type, vax_date = max_date), by = id]
dt_vax_long_trim_extended <- rbindlist(list(dt_vax_long_trim, extra_rows), fill = TRUE)
setorder(dt_vax_long_trim_extended, id, dose)

# Reordering IDs based on their minimum vaccination date, for plotting
dt_vax_plot <- dt_vax_long_trim_extended[
  , id := fct_reorder(
  factor(id), vax_date, .fun = min)]

# Plotting timings of vaccines
p_timings <- ggplot() +
  geom_line(
    data = dt_vax_plot,
    aes(x = vax_date,
        y = id,
        group = id,
        colour = factor(dose)), size = 1.2) +
  geom_point(data = dt_all_data_plot[, .(date = unique(date)), by = id],
             aes(x = date, y = id), size = 1, shape = 2) +
    geom_vline(aes(xintercept = date_delta), linetype = "dashed") +
    geom_vline(aes(xintercept = date_ba2), linetype = "dashed") +
    geom_vline(aes(xintercept = date_xbb), linetype = "dashed") +
    theme_linedraw() +
    theme(axis.text.y=element_blank(),
          legend.position = "bottom",
          text = element_text(size = 9, family = "Helvetica"),
          strip.placement = "outside",
          plot.title = element_text(face = "plain", size = 9),
          legend.box = "vertical",
          legend.margin = margin(),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(colour = 'black')) +
    scale_x_date(date_labels = "%b %Y") +
  scale_colour_nejm() +
  labs(x = "Date", y = "Participant", colour = "Vaccination")

print(p_timings)

ggsave("outputs/figures/supplementary_figures/vaccine_timings.png",
       p_timings,
       width = 10,
       height = 16,
       bg = "white")

#####################################################
## FIGURE S2 - VERSION OF FIGURE 2 WITH ALL PANELS ##
## NEED TO RUN FIRST HALF OF FIGURE 2 SCRIPT FIRST ##
#####################################################

p_figure_2_all_panels <- build_figure_2(
  dt_all_data_plot[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_all_fits_trunc[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_all_fits_trunc[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  facet_formula = facet_formula,
  dt_emergence = dt_emergence,
  dt_dominance = dt_dominance,
  dt_dates = dt_dates,
  t_max = 150,
  alpha_data_pre = 0.5,
  alpha_data_post = 0.25,
  alpha_fits_pre = 0.65,
  alpha_fits_post = 0.15,
  manual_facet_order = c("Delta wave", "BA.2 wave", "XBB wave"),
  plot_beyond = TRUE,
  plot_data = FALSE) +
  scale_colour_manual(values = manual_pal_figure) +
  scale_fill_manual(values = manual_pal_figure) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9))

ggsave("outputs/figures/supplementary_figures/figure_2_all_panels.png",
       p_figure_2_all_panels,
       width = 8,
       height = 6,
       bg = "white")

############################################################
## FIGURE S3 - VERSION OF FIGURE 2 WITHOUT REAL-TIME FITS ##
############################################################

p_figure_2_all_panels_full_only <-build_figure_2(
  dt_all_data_plot[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_all_fits_full[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_all_fits_full[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  facet_formula = facet_formula,
  dt_emergence = dt_emergence,
  dt_dominance = dt_dominance,
  dt_dates = dt_dates,
  t_max = 150,
  alpha_data_pre = 0.5,
  alpha_data_post = 0.5,
  alpha_fits_pre = 0.0,
  alpha_fits_post = 0.5,
  manual_facet_order = c("Delta wave", "BA.2 wave", "XBB wave"),
  plot_beyond = TRUE,
  plot_data = FALSE) +
  scale_colour_manual(values = manual_pal_figure) +
  scale_fill_manual(values = manual_pal_figure) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9))

ggsave("outputs/figures/supplementary_figures/figure_2_all_panels_full_only.png",
       p_figure_2_all_panels_full_only,
       width = 8,
       height = 6,
       bg = "white")

###########################################
## FIGURE S3 - ALL POPULATION-POSTERIORS ##
###########################################

dt_pop_posterior_delta <- extract_parameters_pop_clean(
  fit_delta_full, dt_data = dt_delta_data_full,
  dt_data_stan = dt_delta_full_stan, stan_data = stan_data_delta_full,
  formula = formula_delta, format = "long")

dt_pop_posterior_ba2 <- extract_parameters_pop_clean(
  fit_ba2_full, dt_data = dt_ba2_data_full,
  dt_data_stan = dt_ba2_full_stan, stan_data = stan_data_ba2_full,
  formula = formula_ba2, format = "long")

dt_pop_posterior_xbb <- extract_parameters_pop_clean(
  fit_xbb_full, dt_data = dt_xbb_data_full,
  dt_data_stan = dt_xbb_full_stan, stan_data = stan_data_xbb_full,
  formula = formula_xbb, format = "long")

dt_pop_posterior_all <- rbind(
  dt_pop_posterior_delta[, Wave := "Delta wave"],
  dt_pop_posterior_ba2[, Wave := "BA.2 wave"],
  dt_pop_posterior_xbb[, Wave := "XBB wave"])

dt_pop_posterior_all[variable == "t0_pop", variable := "Initial titre"]
dt_pop_posterior_all[variable == "tp_pop", variable := "Time of peak"]
dt_pop_posterior_all[variable == "ts_pop", variable := "Time of set point"]
dt_pop_posterior_all[variable == "m1_pop", variable := "Boost gradient"]
dt_pop_posterior_all[variable == "m2_pop", variable := "1st wane gradient"]
dt_pop_posterior_all[variable == "m3_pop", variable := "2nd wane gradient"]

dt_pop_posterior_all[variable == "beta_t0", variable := "T0 effect size"]
dt_pop_posterior_all[variable == "beta_tp", variable := "tp effect size"]
dt_pop_posterior_all[variable == "beta_ts", variable := "ts effect size"]
dt_pop_posterior_all[variable == "beta_m1", variable := "m1 effect size"]
dt_pop_posterior_all[variable == "beta_m2", variable := "m2 effect size"]
dt_pop_posterior_all[variable == "beta_m3", variable := "m3 effect size"]

p_population_posteriors <- dt_pop_posterior_all[, Wave := fct_relevel(Wave, "Delta wave")][
  , `Infection history` := fct_relevel(
    `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |>
  ggplot() +
  geom_density_ridges(aes(
    x = value, y = factor(`Titre type`),
    group = interaction(Wave, `Titre type`), fill = factor(`Titre type`))) +
  facet_nested(`Infection history` ~ variable , scales = "free") +
  scale_fill_manual(values = manual_pal_figure) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9)) +
  labs(x = "Value", y = "Titre type")

ggsave("outputs/figures/supplementary_figures/pop_posteriors.png",
       p_population_posteriors,
       width = 12,
       height = 6,
       bg = "white")


#########################################
## FIGURE S5 - LESS INFORMATIVE PRIORS ##
#########################################

# Prior values are set in retrieve_stan_data() function by the call to
# set_prior_values(). We re-run the model for the full Delta, BA.2 and XBB
# datasets here and plot the results after increasing the individual-level
# variation parameters

# Getting data ready for Stan. Less informative priors set using the
# retrieve_stan_data() function with custom sigma values set
stan_data_delta_full <- retrieve_stan_data(
  dt_delta_full_stan, time_type = "relative",
  formula_delta,
  prior_sigma_values = c(10, 10, 10, 1, 1, 1),
  default_prior_values = FALSE)

stan_data_ba2_full <- retrieve_stan_data(
  dt_ba2_full_stan, time_type = "relative",
  formula_ba2,
  prior_sigma_values = c(10, 10, 10, 1, 1, 1),
  default_prior_values = FALSE)

stan_data_xbb_full <- retrieve_stan_data(
  dt_xbb_full_stan, time_type = "relative",
  formula_xbb,
  prior_sigma_values = c(10, 10, 10, 1, 1, 1),
  default_prior_values = FALSE)

#--- Compiling model
mod <- cmdstan_model(
  "stan/antibody_kinetics_main.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE))

# Fitting model to three wave datasets
fit_delta_less_informative <- mod$sample(
  data = stan_data_delta_full,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

fit_ba2_less_informative <- mod$sample(
  data = stan_data_ba2_full,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

fit_xbb_less_informative <- mod$sample(
  data = stan_data_xbb_full,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

# Saving fits
fit_delta_less_informative$save_object("outputs/fits/delta_less_informative.rds")
fit_ba2_less_informative$save_object("outputs/fits/ba2_less_informative.rds")
fit_xbb_less_informative$save_object("outputs/fits/xbb_less_informative.rds")

# Extracting posterior samples and simulating population-level trajectories
dt_delta_fits_less_informative <- process_fits(
  fit_delta_less_informative,
  dt_stan = dt_delta_full_stan,
  stan_data = stan_data_delta_full,
  formula_delta, t_max = 150)

dt_ba2_fits_less_informative <- process_fits(
  fit_ba2_less_informative,
  dt_stan = dt_ba2_full_stan,
  stan_data = stan_data_ba2_full,
  formula_ba2, t_max = 150)

dt_xbb_fits_less_informative <- process_fits(
  fit_xbb_less_informative,
  dt_stan = dt_xbb_full_stan,
  stan_data = stan_data_xbb_full,
  formula_xbb, t_max = 150)

# Putting fits together ready to be plot
dt_fits_less_informative <- rbind(
  dt_delta_fits_less_informative[, Wave := "Delta wave"],
  dt_ba2_fits_less_informative[, Wave := "BA.2 wave"],
  dt_xbb_fits_less_informative[, Wave := "XBB wave"])[
    , Wave := factor(Wave)][
      `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
        , `Titre type` := paste0(`Titre type`, " Abs")][
          , `Titre type` := fct_relevel(`Titre type`, c(
            "Ancestral Abs", "Alpha Abs", "Delta Abs",
            "BA.1 Abs", "BA.2 Abs",
            "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

# Plotting fits using same format as Figure 2
p_figure_2_less_informative <- build_figure_2(
  dt_all_data_plot[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_fits_less_informative[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_fits_less_informative[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  facet_formula = facet_formula,
  dt_emergence = dt_emergence,
  dt_dominance = dt_dominance,
  dt_dates = dt_dates,
  t_max = 150,
  alpha_data_pre = 0.5,
  alpha_data_post = 0.5,
  alpha_fits_pre = 0.0,
  alpha_fits_post = 0.5,
  manual_facet_order = c("Delta wave", "BA.2 wave", "XBB wave"),
  plot_beyond = TRUE,
  plot_data = TRUE) +
  scale_colour_manual(values = manual_pal_figure) +
  scale_fill_manual(values = manual_pal_figure) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9))

# Saving figure
ggsave("outputs/figures/supplementary_figures/less_informative.png",
       p_figure_2_less_informative,
       width = 8,
       height = 6,
       bg = "white")

####################################################
## FIGURE S6 - GREATER INDIVIDUAL-LEVEL VARIATION ##
####################################################

# Getting data ready for Stan. Greater individual-level variation set
# using a different Stan model. Make this more flexible in the future with
# individual-level variation set as an input
stan_data_delta_full <- retrieve_stan_data(
  dt_delta_full_stan, time_type = "relative",
  formula_delta)

stan_data_ba2_full <- retrieve_stan_data(
  dt_ba2_full_stan, time_type = "relative",
  formula_ba2)

stan_data_xbb_full <- retrieve_stan_data(
  dt_xbb_full_stan, time_type = "relative",
  formula_xbb)

# Compiliing model
mod_higher_ind <- cmdstan_model(
  "stan/antibody_kinetics_higher_ind.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE))

# Fitting model to three waves of data
fit_delta_full_higher_ind <- mod_higher_ind$sample(
  data = stan_data_delta_full,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

fit_ba2_full_higher_ind <- mod_higher_ind$sample(
  data = stan_data_ba2_full,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

fit_xbb_full_higher_ind <- mod_higher_ind$sample(
  data = stan_data_xbb_full,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

# Saving fits
fit_delta_full_higher_ind$save_object("outputs/fits/delta_higher_ind.rds")
fit_ba2_full_higher_ind$save_object("outputs/fits/ba2_higher_ind.rds")
fit_xbb_full_higher_ind$save_object("outputs/fits/xbb_higher_ind.rds")

# Extracting posterior samples and simulating population-level trajectories
dt_delta_fits_higher_ind <- process_fits(
  fit_delta_full_higher_ind,
  dt_stan = dt_delta_full_stan,
  stan_data = stan_data_delta_full,
  formula_delta, t_max = 150)

dt_ba2_fits_higher_ind <- process_fits(
  fit_ba2_full_higher_ind,
  dt_stan = dt_ba2_full_stan,
  stan_data = stan_data_ba2_full,
  formula_ba2, t_max = 150)

dt_xbb_fits_higher_ind <- process_fits(
  fit_xbb_full_higher_ind,
  dt_stan = dt_xbb_full_stan,
  stan_data = stan_data_xbb_full,
  formula_xbb, t_max = 150)

# Putting fits together ready to be plot
dt_fits_higher_ind <- rbind(
  dt_delta_fits_higher_ind[, Wave := "Delta wave"],
  dt_ba2_fits_higher_ind[, Wave := "BA.2 wave"],
  dt_xbb_fits_higher_ind[, Wave := "XBB wave"])[
    , Wave := factor(Wave)][
      `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
        , `Titre type` := paste0(`Titre type`, " Abs")][
          , `Titre type` := fct_relevel(`Titre type`, c(
            "Ancestral Abs", "Alpha Abs", "Delta Abs",
            "BA.1 Abs", "BA.2 Abs",
            "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

# Plotting fits using same format as Figure 2
p_figure_2_higher_ind <- build_figure_2(
  dt_all_data_plot[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_fits_higher_ind[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  dt_fits_higher_ind[, `Infection history` := fct_relevel(`Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique(),
  facet_formula = facet_formula,
  dt_emergence = dt_emergence,
  dt_dominance = dt_dominance,
  dt_dates = dt_dates,
  t_max = 150,
  alpha_data_pre = 0.5,
  alpha_data_post = 0.5,
  alpha_fits_pre = 0.0,
  alpha_fits_post = 0.5,
  manual_facet_order = c("Delta wave", "BA.2 wave", "XBB wave"),
  plot_beyond = TRUE,
  plot_data = TRUE) +
  scale_colour_manual(values = manual_pal_figure) +
  scale_fill_manual(values = manual_pal_figure) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9))

# Saving figure
ggsave("outputs/figures/supplementary_figures/higher_ind.png",
       p_figure_2_higher_ind,
       width = 8,
       height = 6,
       bg = "white")

###################################################
## FIGURE S7 - MONOVALENT VS BIVALENT - FIGURE 2 ##
###################################################

# Getting data ready for Stan. Greater individual-level variation set
# using a different Stan model. Make this more flexible in the future with
# individual-level variation set as an input
monovalent_vax <- c("BNT162b2", "mRNA1273")
bivalent_vax <- c("mRNA1273.214", "BNT162b2+BA1")

dt_xbb_bivalent <- dt_xbb_full[
  !is.na(last_vax_type)][
  last_vax_type != "others"]

dt_xbb_bivalent[last_vax_type %in% monovalent_vax, vaccine_type := "Monovalent"]
dt_xbb_bivalent[last_vax_type %in% bivalent_vax, vaccine_type := "Bivalent"]

dt_xbb_bivalent[, vaccine_type := factor(vaccine_type)]

# Need new IDs after filtering out some NA and other vax types
dt_xbb_bivalent[, stan_id := .GRP, by = stan_id]

formula_xbb_bivalent <-  ~ 0 + vaccine_type

dt_xbb_bivalent_stan <- prepare_stan_data(dt_xbb_bivalent)

stan_data_xbb_bivalent <- retrieve_stan_data(
  dt = dt_xbb_bivalent_stan,
  time_type = "relative",
  formula_xbb_bivalent)

# Compiliing model
mod <- cmdstan_model(
  "stan/antibody_kinetics_main.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE))

fit_xbb_bivalent <- mod$sample(
  data = stan_data_xbb_bivalent,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

fit_xbb_bivalent$save_object("outputs/fits/xbb_bivalent.rds")

dt_xbb_fits_bivalent <- process_fits(
  fit_xbb_bivalent,
  dt_stan = dt_xbb_bivalent_stan,
  stan_data = stan_data_xbb_bivalent,
  formula_xbb_bivalent, t_max = 150,
  cleaned_names = c(
    "Vaccine type", "Titre type"))

setnames(dt_xbb_bivalent, "titre_type", "Titre type")
# setnames(dt_xbb_bivalent, "infection_history", "Infection history")

# Putting fits together ready to be plot
dt_fits_bivalent <- dt_xbb_bivalent_stan[
    , Wave := factor(Wave)][
      `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
        , `Titre type` := paste0(`Titre type`, " Abs")][
          , `Titre type` := fct_relevel(`Titre type`, c(
            "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

p_figure_2_bivalent <- dt_xbb_fits_bivalent[
  , `Vaccine type` := fct_relevel(`Vaccine type`, "Monovalent")] |>
  ggplot() +
  geom_line(aes(x = t, y = me, colour = `Vaccine type`)) +
  geom_ribbon(aes(
    x = t, ymin = lo, ymax = hi, fill = `Vaccine type`),
    alpha = 0.3) +
  geom_hline(aes(yintercept = 40),
            linetype = "dotdash", colour = "gray30", alpha = 0.4) +
  geom_hline(aes(yintercept = 2560),
            linetype = "dotdash", colour = "gray30", alpha = 0.4) +
  facet_grid( ~ `Titre type`) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280,
               2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120),
                     labels = c("0", "30", "60", "90", "120"),
                     expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Time since last exposure (days)",
       y = expression(paste("Titre (IC"[50], ")")),
       title = "Monovalent vs Bivalent/BA.1 containing vaccines") +
  theme(
    ggh4x.facet.nestline = element_line(),
    legend.position = "bottom",
    strip.text.x.top = element_text(size = 8, family = "Helvetica"),
    strip.text.x = element_text(size = 8, family = "Helvetica")) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))

# Saving figure
ggsave("outputs/figures/supplementary_figures/monovalent_vs_bivalent_figure_2.png",
       p_figure_2_bivalent,
       width = 8,
       height = 6,
       bg = "white")

###################################################
## FIGURE S8 - MONOVALENT VS BIVALENT - FIGURE 3 ##
###################################################

dt_pop_params_xbb_bivalent <- figure_3_data(
  fit_xbb_bivalent, dt_xbb_bivalent_stan,
  stan_data_xbb_bivalent, formula_xbb_bivalent,
  wave_manual = "XBB wave",
  cleaned_names = c("Vaccine type", "Titre type"))

dt_figure_3_bivalent <- dt_pop_params_xbb_bivalent[
  , rel_drop := mu_s/mu_p,
  by = .(`Vaccine type`, `Titre type`)][
    , `:=` (
      rel_drop_me = quantile(rel_drop, 0.5),
      mu_p_me = quantile(mu_p, 0.5),
      mu_s_me = quantile(mu_s, 0.5)),
    by = .(`Vaccine type`, `Titre type`)]

dt_figure_3_bivalent[
  `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
        , `Vaccine type` := fct_relevel(`Vaccine type`, "Monovalent")][
        , `Titre type` := paste0(`Titre type`, " Abs")][
          , `Titre type` := fct_relevel(`Titre type`, c(
            "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

dt_figure_3_bivalent_point <- dt_figure_3_bivalent[, .(
  `Vaccine type`, `Titre type`,
  rel_drop_me, mu_p_me, mu_s_me)][
    order(`Vaccine type`)] |>
  unique()

manual_pal_figure <-
  c("#CC6677",
    "#DDCC77",
    "#88CCEE",
    "#882255",
    "#44AA99",
    "grey",
    "#D95F02",
    "#66A61E")

p_figure_3_bivalent <- dt_figure_3_bivalent |>
  ggplot(aes(
    x = mu_p, y = mu_s,
    colour = `Vaccine type`)) +
  geom_density_2d(
    aes(
      group = interaction(
        `Vaccine type`,
        `Titre type`))) +
  geom_point(data = dt_figure_3_bivalent[.draw <= 2000],
             alpha = 0.05, size = 0.2) +
  geom_point(data = dt_figure_3_bivalent_point,
             aes(x = mu_p_me, y = mu_s_me,
                 shape = `Titre type`),
             colour = "black") +
  geom_path(data = dt_figure_3_bivalent_point,
            aes(x = mu_p_me, y = mu_s_me,
                group = `Titre type`),
            colour = "black") +
  # geom_vline(xintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_vline(xintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280", "2560"),
    limits = c(NA, 10240)) +
  # geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280", "2560")) +
  # facet_nested(~, scales = "fixed") +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = expression(paste("Population-level titre value at peak (IC"[50], ")")),
       y = expression(paste("Population-level titre value at set-point (IC"[50], ")")),
       title = "Monovalent vs Bivalent/BA.1 containing vaccines") +
  # scale_colour_manual(values = manual_pal_figure) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1)))

ggsave("outputs/figures/supplementary_figures/monovalent_vs_bivalent_figure_3.png",
       p_figure_3_bivalent,
       width = 6,
       height = 6,
       bg = "white")

##################################################
## FIGURE S9 - PFIZER VS ASTRAZENECA - FIGURE 2 ##
##################################################

# Getting data ready for Stan. Greater individual-level variation set
# using a different Stan model. Make this more flexible in the future with
# individual-level variation set as an input
monovalent_vax <- c("BNT162b2", "AZD1222")
bivalent_vax <- c("mRNA1273.214", "BNT162b2+BA1")

dt_delta_vax_type <- dt_delta_data_full[
  !is.na(last_vax_type)][
    last_vax_type != "others"][
      last_vax_type != "mRNA1273"][
        , last_vax_type := fct_drop(last_vax_type)]

dt_delta_vax_type[, vaccine_type := factor(last_vax_type)]

formula_delta_vax_type <- ~ 0 + vaccine_type

dt_delta_vax_type_stan <- prepare_stan_data(dt_delta_vax_type)

stan_data_delta_vax_type <- retrieve_stan_data(
  dt = dt_delta_vax_type_stan,
  time_type = "relative",
  formula_xbb_bivalent)

# Compiliing model
mod <- cmdstan_model(
  "stan/antibody_kinetics_main.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE))

fit_delta_vax_type <- mod$sample(
  data = stan_data_delta_vax_type,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  threads_per_chain = 4)

fit_delta_vax_type$save_object("outputs/fits/delta_vax_type.rds")

dt_delta_vax_type_fits <- process_fits(
  fit_delta_vax_type,
  dt_stan = dt_delta_vax_type_stan,
  stan_data = stan_data_delta_vax_type,
  formula_delta_vax_type, t_max = 150,
  cleaned_names = c(
    "Vaccine type", "Titre type"))

# Putting fits together ready to be plot
dt_delta_vax_type_fits <- dt_delta_vax_type_fits[
      , `Titre type` := paste0(`Titre type`, " Abs")][
        , `Titre type` := fct_relevel(`Titre type`, c(
          "Ancestral Abs", "Alpha Abs", "Delta Abs"))]

p_figure_2_delta_vax_type <- dt_delta_vax_type_fits |>
  ggplot() +
  geom_line(aes(x = t, y = me, colour = `Vaccine type`)) +
  geom_ribbon(aes(
    x = t, ymin = lo, ymax = hi, fill = `Vaccine type`),
    alpha = 0.3) +
  geom_hline(aes(yintercept = 40),
             linetype = "dotdash", colour = "gray30", alpha = 0.4) +
  geom_hline(aes(yintercept = 2560),
             linetype = "dotdash", colour = "gray30", alpha = 0.4) +
  facet_grid( ~ `Titre type`) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280,
               2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120),
                     labels = c("0", "30", "60", "90", "120"),
                     expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Time since last exposure (days)",
       y = expression(paste("Titre (IC"[50], ")")),
       title = "AZD1222 vs BNT162b2 during Delta wave") +
  theme(
    ggh4x.facet.nestline = element_line(),
    legend.position = "bottom",
    strip.text.x.top = element_text(size = 8, family = "Helvetica"),
    strip.text.x = element_text(size = 8, family = "Helvetica")) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))

ggsave("outputs/figures/supplementary_figures/delta_vax_type_figure_2.png",
       p_figure_2_delta_vax_type,
       width = 8,
       height = 6,
       bg = "white")

###################################################
## FIGURE S10 - PFIZER VS ASTRAZENECA - FIGURE 3 ##
###################################################

dt_pop_params_delta_vax_type <- figure_3_data(
  fit_delta_vax_type, dt_delta_vax_type,
  stan_data_delta_vax_type, formula_delta_vax_type,
  wave_manual = "Delta wave",
  cleaned_names = c("Vaccine type", "Titre type"))

dt_figure_3_vax_type <- dt_pop_params_delta_vax_type[
  , rel_drop := mu_s/mu_p,
  by = .(`Vaccine type`, `Titre type`)][
    , `:=` (
      rel_drop_me = quantile(rel_drop, 0.5),
      mu_p_me = quantile(mu_p, 0.5),
      mu_s_me = quantile(mu_s, 0.5)),
    by = .(`Vaccine type`, `Titre type`)]

dt_figure_3_vax_type[
  , `Titre type` := fct_relevel(
    `Titre type`, c(
      "Ancestral", "Alpha", "Delta"))][
          , `Titre type` := paste0(`Titre type`, " Abs")][
            , `Titre type` := fct_relevel(`Titre type`, c(
              "Ancestral Abs", "Alpha Abs", "Delta Abs"))]

dt_figure_3_vax_type_points <- dt_figure_3_vax_type[, .(
  `Vaccine type`, `Titre type`,
  rel_drop_me, mu_p_me, mu_s_me)][
    order(`Vaccine type`)] |>
  unique()

p_figure_3_delta_vax_type <- dt_figure_3_vax_type |>
  ggplot(aes(
    x = mu_p, y = mu_s,
    colour = `Vaccine type`)) +
  geom_density_2d(
    aes(
      group = interaction(
        `Vaccine type`,
        `Titre type`))) +
  geom_point(data = dt_figure_3_vax_type[.draw <= 2000],
             alpha = 0.05, size = 0.2) +
  geom_point(data = dt_figure_3_vax_type_points,
             aes(x = mu_p_me, y = mu_s_me,
                 shape = `Titre type`),
             colour = "black") +
  geom_path(data = dt_figure_3_vax_type_points,
            aes(x = mu_p_me, y = mu_s_me,
                group = `Titre type`),
            colour = "black") +
  # geom_vline(xintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_vline(xintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280", "2560")) +
  # geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280", "2560")) +
  # facet_nested(~, scales = "fixed") +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = expression(paste("Population-level titre value at peak (IC"[50], ")")),
       y = expression(paste("Population-level titre value at set-point (IC"[50], ")")),
       title = "AZD1222 vs BNT162b2 during Delta wave") +
  # scale_colour_manual(values = manual_pal_figure) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1)))

ggsave("outputs/figures/supplementary_figures/delta_vax_type_figure_3.png",
       p_figure_3_delta_vax_type,
       width = 6,
       height = 6,
       bg = "white")


#######################################################################
## FIGURE SX DEPRECATED - POPULATION-POSTERIORS VS POPULATION PRIORS ##
#######################################################################

# dt_pop_posterior_all_plot <- dt_pop_posterior_all[, Wave := fct_relevel(Wave, "Delta wave")][
#   , `Infection history` := fct_relevel(
#     `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))]
#
# dt_pop_priors <- sample_pop_priors(n_samples = 4000)
# dt_pop_priors[, `Titre type` := "Prior"]
# setnames(dt_pop_priors, "sample_id", ".draw")
#
# dt_pop_priors_long <- melt(dt_pop_priors, id.vars = c(".draw", "Titre type"))
#
# dt_pop_priors_long[variable == "t0", variable := "Initial titre"]
# dt_pop_priors_long[variable == "tp", variable := "Time of peak"]
# dt_pop_priors_long[variable == "ts", variable := "Time of set point"]
# dt_pop_priors_long[variable == "m1", variable := "Boost gradient"]
# dt_pop_priors_long[variable == "m2", variable := "1st wane gradient"]
# dt_pop_priors_long[variable == "m3", variable := "2nd wane gradient"]
#
# dt_pop_priors_long[variable == "beta_t0", variable := "T0 effect size"]
# dt_pop_priors_long[variable == "beta_tp", variable := "tp effect size"]
# dt_pop_priors_long[variable == "beta_ts", variable := "ts effect size"]
# dt_pop_priors_long[variable == "beta_m1", variable := "m1 effect size"]
# dt_pop_priors_long[variable == "beta_m2", variable := "m2 effect size"]
# dt_pop_priors_long[variable == "beta_m3", variable := "m3 effect size"]
#
# # Add "Prior" as the Titre type for all priors
# dt_pop_priors_long[, `Titre type` := "Prior"]
#
# # Get the unique combinations of Infection history and variable from the posterior data
# unique_combinations <- unique(dt_pop_posterior_all_plot[, .(`Infection history`, variable, Wave)])
#
# # Cross join unique combinations with priors
# cross_joined <- unique_combinations[dt_pop_priors_long, on = "variable", allow.cartesian = TRUE]
#
# # Now, rbind this with the original posteriors data.table, the rows will align based on the repeated priors for each Infection history and variable
# combined_dt <- rbind(dt_pop_posterior_all_plot, cross_joined)
#
# ggplot() +
#   geom_density_ridges(
#     data = combined_dt,
#     aes(x = value, y = factor(`Titre type`),
#         fill = factor(`Titre type`),
#         group = interaction(`Titre type`))) +
#   facet_nested(`Infection history` ~ variable , scales = "free") +
#   scale_fill_manual(values = c(manual_pal_figure, "grey30")) +
#   theme_linedraw() +
#   theme(legend.position = "none",
#         text = element_text(size = 8, family = "Helvetica"),
#         strip.background =element_rect(fill="white"),
#         strip.text = element_text(colour = 'black'),
#         strip.placement = "outside",
#         plot.title = element_text(face = "bold", size = 9)) +
#   labs(x = "Value", y = "Titre type")
