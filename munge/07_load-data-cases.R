# CASES
case_counts <- preprocess_outcome(file = "COVID19Cases_geoRegion_AKL10_w.csv", df = FALSE)
data_cases <- preprocess_outcome(file = "COVID19Cases_geoRegion_AKL10_w.csv", df = TRUE)

save(case_counts,
     file = "output/cases.Rdata")
