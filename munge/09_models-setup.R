## Age groups are not aggregated
GROUPING_a <- rep(1, dim(case_counts)[3] - 1)
GROUPING_r <- rep(1, dim(case_counts)[2] - 1)

# a surveillance time series object by age and region
# -- units are "region.agegroup" (region varies faster)
covidCH_sts_both <- covidCH(counts = case_counts, popCH = popCH,
                            map = mapCH, by = "all", flatten = TRUE,
                            agegroups = GROUPING_a)
# covidCH_sts_both as a list of NAGEGROUPS "sts" objects
# -> (this is what the flatten argument does)
#covidCH_sts_rbya <- covidCH(counts = case_counts, popCH = popCH,
#                      map = mapCH, by = "all", flatten = FALSE,
#                       agegroups = GROUPING_a)
# a surveillance time series aggregated over all age groups
covidCH_sts_r <- covidCH(counts = case_counts, popCH = popCH,
                         map = mapCH, by = "regions")
# a surveillance time series aggregated across regions
covidCH_sts_a <- covidCH(counts = case_counts, popCH = popCH,
                         map = mapCH, by = "agegroups")
## matrix of corresponding population counts (region x age group)
popCH_both <- aggregateCountsArray(popCH, dim = 2, grouping = GROUPING_a)

## names of groups and regions
REGIONS <- unique(stratum(covidCH_sts_both, 1))
NREGIONS <- length(REGIONS)
AGEGROUPS <- unique(stratum(covidCH_sts_both, 2))
NAGEGROUPS <- length(AGEGROUPS)