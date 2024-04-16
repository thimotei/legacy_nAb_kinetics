# Real-time estimation of immunological responses against emerging SARS-CoV-2 variants

This is the codebase for the version of the underlying model, the publicly available version of the dataset and the
scripts to perform all of the analyses. The codebase focuses on scripts to reproduce our specific analysis, but there are plans to develop the key inference framework into a package.

## Overfile of repository

### Antibody kinetics model code
The underlying antibody kinetics model is written in [Stan](https://mc-stan.org/). We use [cmdstan](https://mc-stan.org/users/interfaces/cmdstan), called from [R](https://www.r-project.org/) using the [cmdstanr](https://mc-stan.org/cmdstanr/) package. We use [data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html) for our data manipulation throughout and [ggplot2](https://ggplot2.tidyverse.org/) for all plots. Lastly, for extracting the posterior samples in a user-friendly and simple manner, we ubiquitously use the [tidybayes](https://mjskay.github.io/tidybayes/) package.

The model used throughout the main analysis can be found at `stan/antibody_kinetics_main.stan`.

### Inference scripts
For the results used in the main text, it is called three times, for each of the three waves of vaccinations/infections under investigation: Delta, BA.2 and XBB. These three scripts can be found in `scripts/inference`.

### Plotting scripts
Scripts to reproduce the 4 figures in the main text can be found at `scripts/figures`. Each loads the required data and/or fits, munges the data and fits into precisely the right format for the plot, then creates and saves the plot. Figure 4 requires the script `scripts/figures/figure_4_data.R` to be run before the plot can be created. This is because Figure 4 manipulates the individual-level nAb kinetics, of which there are many. To simulate these takes some time. However, we save the resulting file as an .RDS file at `figure_4_data.RDS`, so the user does not have to re-run this script, which takes some time and requires high amounts of RAM.  

### Workflow

The workflow to reproduce the results in the main text can be summarised as follows:

0. Process the Legacy data object. The code used to do this is available (at `scripts/setup/preprocess_data.R`), but the original raw data cannot be made publicly available. However, the processed data is included, and this has everything required to run the full inference for the results in both the main text and supplementary material.

1. Load the six datasets required to run the six different inferences. The six data files can be found in `data/` and have the following format: `wave_trunc/full.rds`. The `trunc/full` refers to whether the dataset is the full dataset, or whether it was truncated at the date of the emergence of the variant in question. 

2. Generate the figures from the main text by running `scripts/figures/generate_figures.R`. At the start, these calls the inference scripts within `scripts/inference`. Specifically, the inference code for the Delta, BA.2 and XBB waves are `delta.R`, `ba2.R` and `xbb.R`. Each script fits the model twice, once to the full set of data for that wave, and once with just the truncated data, representing the real-time fit. The end of each of these three scripts saves the resulting model fit, at `outputs/fits`. The saved model fit objects are large and are therefore included in this public repository, meaning that any analysis that uses the fit objects requires the user to refit the model. Once the inference has run once, the scripts check for the existence of the fit objects and they do not re-run the inference if the scripts are sourced again.

3. For the results in the supplementary material, run the script `scripts/figures/supplement/supplementary_figures.R`. 