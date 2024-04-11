# Real-time estimation of immunological responses against emerging SARS-CoV-2 variants

This is the codebase for the version of the underlying model, the publicly available version of the dataset and the
scripts to perform all of the analyses in [this paper]. The codebase is not quite a package as it stands, as it directly accompanys one specific analysis. However, we plan for the development codebase hosted at [this repository](https://github.com/thimotei/legacy_correlates) to be turned into a package, allowing for slightly easier installation.

## Quick summary

### Antibody kinetics model code
The underlying antibody kinetics model is written in [Stan](https://mc-stan.org/). We use [cmdstan](https://mc-stan.org/users/interfaces/cmdstan), called from [R](https://www.r-project.org/) using the [cmdstanr](https://mc-stan.org/cmdstanr/) package. We use [data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html) for our data manipulation throughout and [ggplot2](https://ggplot2.tidyverse.org/) for all plots. Lastly, for extracting the posterior samples in a user-friendly and simple manner, we ubiquitously use the [tidybayes](https://mjskay.github.io/tidybayes/) package.

The model used throughout the main analysis can be found at `stan/antibody_kinetics_main.stan`.

### Inference scripts
For the results used in the main text, it is called three times, for each of the three waves of vaccinations/infections under investigation: Delta, BA.2 and XBB. These three scripts can be found in `scripts/inference`.

### Plotting scripts
Scripts to reproduce the 4 figures in the main text can be found at `scripts/figures`. Each loads the required data and/or fits, munges the data and fits into precisely the right format for the plot, then creates and saves the plot. Figure 4 requires the script `scripts/figures/figure_4_data.R` to be run before the plot can be created. This is because Figure 4 manipulates the individual-level nAb kinetics, of which there are many. To simulate these takes some time. However, we save the resulting file as an .RDS file at `figure_4_data.RDS`, so the user does not have to re-run this script, which takes some time and requires high amounts of RAM.  

### Workflow

The workflow to reproduce the results in the main text can be summarised as follows:

0). Process the Legacy data object. The code to do this is available (at `scripts/setup/preprocess_data.R`), but the original data is not. Only the processed data is publicly available. However, the The publicly available dataset includes everything required to run the full inference for the results in both the main text and supplementary material.

1). Run each of the three inference scripts for the main text. They are all within `scripts/inference`. Speficially, they are `delta.R`, `ba2.R` and `xbb.R`. Each script fits the model twice, once to the full set of data for that wave, and once with just the truncated data, representing the real-time fit. The end of each of these three scripts saves the resulting model fit, at `outputs/fits`. The saved model fit objects are large and are therefore not added to the public repository, meaning that any analysis that uses the fit objects requires the user to refit the model. 

2). Open one of the scripts in the `scripts/figures` directory to reproduce the results in the main text.

3). For the results in the supplementary material, open the script `scripts/figures/supplement/supplementary_figures.R`. 

### Required packages 

We use the following packages throughout.

```r
c("cmdstanr", "data.table", "ggplot2", "lubridate", "forcats", "tidybayes", 
"stringr", "truncnorm")
```