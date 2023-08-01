# ee-vax

This repository contains the code for the analysis presented in [*The COVID-19 vaccination campaign in Switzerland and its impact on disease spread*](https://doi.org/10.1101/2023.04.06.23288251 )

### Ensuring you have the raw data

Please first run the script `download-data-sets.R` such that the correct data is downloaded to your machine

### Conducting the pre-processing of data

Provided you do not wish to use `ProjectTemplate::load.project()`, you should first load the packages listed in config/global.dcf
```
library(knitr)
library(surveillance)
library(hhh4contacts)
library(ggplot2)
library(patchwork)
library(viridis)
library(ggthemes)
library(kableExtra)
library(openxlsx)
library(reshape2)
library(maptools)
library(sp)
library(spdep)
library(sf)
library(tmap)
library(tmaptools)
library(wrapr)
library(zoo)
library(ggforce)
library(hhh4addon)
library(MASS)
```
and then execute in order the following scripts before running the code
```
source("lib/globals.R")
source("lib/function-AIC-correction.R")
source("lib/function-change-vaccine-distribution.R")
source("lib/function-date-to-text.R")
source("lib/function-format-coef-for-table.R")
source("lib/function-Hannah-Quinn.R")
source("lib/function-include-uncertainty-predictions.R")
source("lib/function-iso-fix.R")
source("lib/function-normalise-indicator.R")
source("lib/function-outcome-counts.R")
source("lib/function-plot-matrices.R")
source("lib/function-plot-models.R")
source("lib/function-plot-predictions.R")
source("lib/function-sine-cosine-wave-data.R")
source("lib/function-skip-plot-labels.R")
source("lib/function-surveillance-time-series.R")
source("lib/function-table-models.R")
source("lib/function-waning.R")

source("munge/01_manual-cantons.R")
source("munge/02_manual-adjacency.R")
source("munge/03_load-data-map.R")
source("munge/04_load-data-mobility.R")
source("munge/05_load-data-contact-and-policy.R")
source("munge/06_load-data-vaccines-and-population.R")
source("munge/07_load-data-cases.R")
source("munge/08_create-coverage-covariate.R")
source("munge/09_models-setup.R")
source("munge/10_scenario-analysis-alternative-covariate.R")
source("munge/11_sensitivity-vaccination-coverage.R")
```

### Data we have created

Manually created data sets are contained in the folder [output](output) and described in the folder [metadata](metadata)

### Conducting the analysis

Following this, you should be able to execute the code in folder [reports/code](reports/code), see above if you do not wish to use `load.project()` and ensure you have run the pre-processing scripts first

