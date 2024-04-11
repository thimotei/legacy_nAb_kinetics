# Load libraries ----------------------------------------------------------
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

# Set up and run inference ---------------------------------------------------------------
source("scripts/setup/setup.R")
source("scripts/inference/delta.R")
source("scripts/inference/ba2.R")
source("scripts/inference/xbb.R")

# Load data and outputs ---------------------------------------------------------------
dt_clean <- readRDS("data/processed_data_condensed.rds")

# Sourcing C++ code
sourceCpp("cpp/simulate_trajectories.cpp")
sourceCpp("cpp/convert_log_scale_inverse.cpp")

# Loading fits
fit_delta_trunc <- readRDS("outputs/fits/delta_trunc.rds")
fit_ba2_trunc <- readRDS("outputs/fits/ba2_trunc.rds")
fit_xbb_trunc <- readRDS("outputs/fits/xbb_trunc.rds")

fit_delta_full <- readRDS("outputs/fits/delta_full.rds")
fit_ba2_full <- readRDS("outputs/fits/ba2_full.rds")
fit_xbb_full <- readRDS("outputs/fits/xbb_full.rds")

# Generate figures ---------------------------------------------------------------

source("scripts/figures/figure_1.R")
source("scripts/figures/figure_2.R")
source("scripts/figures/figure_3.R")
source("scripts/figures/figure_4.R")
source("scripts/figures/figure_5.R")