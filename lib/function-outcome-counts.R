#' Preprocess BAG case and hospitalisation data
#' 
#' @description Functionality to preprocess the COVID-19 cases and
#' hospitalisations reported by BAG such that they can be used in
#' endemic-epidemic modelling
#' @param file Name of file to be processed. Must be contained in the data
#' directory
#' @param df Boolean determining whether to return a data frame or not. If TRUE
#' returns a data frame and if FALSE returns array of counts
#' @returns Counts ordered by unit 
preprocess_outcome <- function(file = c("COVID19Cases_geoRegion_AKL10_w.csv",
                                        "COVID19Hosp_geoRegion_AKL10_w.csv"),
                               df){
  if(!is.logical(df)){
    stop("df must be TRUE/FALSE")
  }
  file <- match.arg(file)
  # Load relevant data set
  data_outcome <- read.csv(paste0("data/", file),
                           header = TRUE, stringsAsFactors = TRUE)
  # Ensure only cantons included as regions in data
  # and get rid of unused factor levels
  data_outcome <- data_outcome[!(data_outcome$geoRegion %in% region_exclude), ]
  data_outcome <- data_outcome[!(data_outcome$altersklasse_covid19 %in% age_exclude), ]
  data_outcome$geoRegion <- droplevels(data_outcome$geoRegion)
  # Reformat date to be ISO date with a hyphen
  data_outcome$dateISO <- paste0(substr(data_outcome$datum, 1, 4), "-",
                                 substr(data_outcome$datum, 5, 6))
  # Restrict dates to be in the study period
  data_outcome <- data_outcome[data_outcome$dateISO %in% date_range_full, ]
  # Ensure only known ages are included
  data_outcome <- data_outcome[data_outcome$altersklasse_covid19 != "Unbekannt", ]
  data_outcome$altersklasse_covid19 <- droplevels(data_outcome$altersklasse_covid19)
  # Create the age group and region strata variable
  data_outcome$groups <- paste(data_outcome$geoRegion,
                               data_outcome$altersklasse_covid19, sep = "-")
  if(isTRUE(df)){
    return(data_outcome)
  } else {
  # Ensure correct date ordering
  
  # Obtain the counts grouped by region and age group
  outcome_counts <- xtabs(entries ~ dateISO + geoRegion + altersklasse_covid19,
                          data = data_outcome)
  # In array format
  outcome_counts <- array(outcome_counts,
                          dim = dim(outcome_counts),
                          dimnames = dimnames(outcome_counts))
  return(outcome_counts)
  }
}
