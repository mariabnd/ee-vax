# ee-vax

This folder contains a description of the data sets found in the folders `data` (information from data providers) and `output` (data after processing that is used in our work)

### cases.Rdata

This data object contains the case counts used in modelling given in a 49 x 26 x 8 array (time points x regions x age groups)

### coverage-covariates.Rdata

Tables of vaccination coverage with dimension names for ISO date and unit

* `vax_cov_covariate` -- 49 x 208 table of values for age group and region
* `vax_cov_covariate_a` -- 49 x 8 table of values for age group
* `vax_cov_covariate_r` -- 49 x 26 table of values for region

### coverage-scen.Rdata

Versions of `coverage-covariates.Rdata` created for scenario analysis (denoted by the suffix `avg` for age groups and `max` for regions)

### coverage-sens.Rdata

Versions of `coverage-covariates.Rdata` created for sensitivity analysis (denoted by the suffix `sens` and a number denoting which of the alternative waning functions is used -- see the supporting information or `reports/code/sens.R` for details)

### matrices.Rdata

This data object contains the time-varying

* `contact_mats` a 8 x 8 x 49 array of time-varying contact matrices for our eight (8) age groups
* `movement_mats` a 26 x 26 x 49 array of time-varying mobility matrices for our 26 regions

### vaccination-coverage.Rdata

This data set contains 10192 observations of seven (7) variables

1. `dateISO` -- the ISO week dates in the study period (49 in total)
	* 2020-53, 2021-01, 2021-02, 2021-03, 2021-04, 2021-05, 2021-06, 2021-07, 2021-08, 2021-09, 2021-10, 2021-11, 2021-12, 2021-13, 2021-14, 2021-15, 2021-16, 2021-17, 2021-18, 2021-19, 2021-20, 2021-21, 2021-22, 2021-23, 2021-24, 2021-25, 2021-26, 2021-27, 2021-28, 2021-29, 2021-30, 2021-31, 2021-32, 2021-33, 2021-34, 2021-35, 2021-36, 2021-37, 2021-38, 2021-39, 2021-40, 2021-41, 2021-42, 2021-43, 2021-44, 2021-45, 2021-46, 2021-47, 2021-48
2. `altersklasse_covid19` -- the eight (8) age groups considered
	* 10 - 19
	* 20 - 29
	* 30 - 39
	* 40 - 49
	* 50 - 59
	* 60 - 69
	* 70 - 79
	* 80+
3. `geoRegion` -- the 26 regions considered
	* 10 - 19
	* 20 - 29
	* 30 - 39
	* 40 - 49
	* 50 - 59
	* 60 - 69
	* 70 - 79
	* 80+
4. `groups` -- the 26 x 8 combinations of age and region (208)
5. `vax_cov` -- vaccination coverage by `group`
6. `vax_cov_r` -- vaccination coverage by region
7. `vax_cov_a` -- vaccination coverage by age group

