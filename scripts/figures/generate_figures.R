# Load libraries ---------------------------------------------------------
library(data.table)
library(forcats)
library(ggplot2)
library(ggsci)
library(patchwork)
library(cowplot)
library(lubridate)
library(Rcpp)
library(gridExtra)
library(parallel)
library(ggh4x)
library(cmdstanr)
library(lubridate)
library(tidybayes)
library(stringr)
library(truncnorm)

# Source all functions ----------------------------------------------------
source("R/source.R")
source_all_functions()

# Sourcing C++ functions --------------------------------------------------
sourceCpp("cpp/simulate_trajectories_old.cpp")
# sourceCpp("cpp/simulate_trajectories_parallel.cpp")
sourceCpp("cpp/convert_log_scale_inverse.cpp")

# Load data ---------------------------------------------------------------

# These two data objects are not currently publicly available
# All of the others are
dt_inf <- fread("data/infections.rds")
dt_vax <- fread("data/vaccines.rds")

dt_delta_trunc <- fread("data/delta_trunc.rds")
dt_delta_full <- fread("data/delta_full.rds")
dt_ba2_trunc <- fread("data/ba2_trunc.rds")
dt_ba2_full <- fread("data/ba2_full.rds")
dt_xbb_trunc <- fread("data/xbb_trunc.rds")
dt_xbb_full <- fread("data/xbb_full.rds")

# Setting emergence dates
date_delta <- ymd("2021-05-07")
date_ba2 <- ymd("2022-01-24")
date_xbb <- ymd("2023-01-09")

# Setting covariate formula
covariate_formula <- ~ 0 + infection_history

# Setting custom colour palette for all figures
manual_pal <-
  c("#CC6677",
    "#DDCC77",
    "#88CCEE",
    "#882255",
    "#44AA99",
    "grey",
    "#D95F02",
    "#66A61E")

# Extract wave data and run inference ------------------------------------
# (does not run if fit objects exist already) ----------------------------
source("scripts/inference/delta.R")
source("scripts/inference/ba2.R")
source("scripts/inference/xbb.R")

# Load fits ---------------------------------------------------------------
fit_delta_trunc <- readRDS("outputs/fits/delta_trunc.rds")
fit_ba2_trunc <- readRDS("outputs/fits/ba2_trunc.rds")
fit_xbb_trunc <- readRDS("outputs/fits/xbb_trunc.rds")

fit_delta_full <- readRDS("outputs/fits/delta_full.rds")
fit_ba2_full <- readRDS("outputs/fits/ba2_full.rds")
fit_xbb_full <- readRDS("outputs/fits/xbb_full.rds")

# Generate figures --------------------------------------------------------
source("scripts/figures/figure_1.R")
source("scripts/figures/figure_2.R")
source("scripts/figures/figure_3.R")
source("scripts/figures/figure_4.R")
source("scripts/figures/figure_5.R")
