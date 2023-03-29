#' Create surveillance time series of COVID-19 data to be analysed
#' 
#' This function redefines the `noroBE` function from hhh4contacts package
#' to include a counts argument such that our case data can be used with it
#' @param counts the case counts considered in the analysis
#' @param popCH population counts for Switzerland
#' @param mapCH map of Swiss regions
#' @param by "regions", "agegroups", "all" or "none"
#' @param agegroups size of age groups
#' @param time_range study period
#' @param flatten Defaults to FALSE
#' @returns an stsObj
covidCH <- function(counts = case_counts,
                    popCH = popCH,
                    map = mapCH,
                    # the above are elements from hhh4contacts called within
                    # the function which we now replace with our own
                    by = c("regions", "agegroups", "all", "none"),
                    agegroups = rep(1, 8),#NULL,
                    time_range = date_range_ISO,
                    flatten = FALSE){
  by <- match.arg(by)
  # Subset counts to time range
  stopifnot(!is.na(
    time_range_idx <- match(time_range, dimnames(counts)[[1L]])
  ))
  counts <- counts[time_range_idx[1L] : time_range_idx[2L], , , drop = FALSE]
  start <- as.integer(
    strsplit(dimnames(counts)[[1L]][1], "-", fixed = TRUE)[[1L]]
  )
  # Aggregate groups
  if (by %in% c("agegroups", "all") && !is.null(agegroups)){
    counts <- aggregateCountsArray(counts = counts, dim = 3,
                                   grouping = agegroups)
    popCH <- aggregateCountsArray(counts = popCH, dim = 2,
                                  grouping = agegroups)
  }
  # Neighbourhood structure
  neighbourhood <- if (by %in% c("regions", "all")){
    map_nbOrder
  } else if (by == "agegroups"){
    Cgrouped
  } else { # no stratification
    matrix(NA, 1L, 1L)
  }
  # Constructor function
  makests <- function(observed, populationFrac, neighbourhood, ...){
    ts <- sts(start = start, frequency = 52,
              observed = observed, populationFrac = populationFrac,
              neighbourhood = neighbourhood, ...)
    rownames(ts@observed) <- rownames(observed)
    return(ts)
  }
  # Create surveillance time series for the chosen level of stratification
  if (by == "none"){
    makests(observed = cbind(CH = rowSums(counts)),
            populationFrac = 1, neighbourhood = neighbourhood)
  } else if (by == "regions"){
    makests(observed = rowSums(counts, dims = 2L),
            populationFrac = prop.table(rowSums(popCH)),
            map = map,
            neighbourhood = neighbourhood)
  } else if (by == "agegroups"){
    makests(observed = apply(counts, c(1, 3), sum),
            populationFrac = prop.table(colSums(popCH)),
            neighbourhood = neighbourhood)
  } else if (flatten){ # where regions vary faster than age groups
    ngroups <- dim(counts)[[3L]]
    nregions <- dim(counts)[[2L]]
    ## replicate 'neighbourhood' block ngroup times
    ## replicate columns
    neighbourhood <- rep.int(neighbourhood, ngroups)
    dim(neighbourhood) <- c(nregions, nregions * ngroups)
    ## replicate rows
    neighbourhood <- do.call("rbind", rep_len(
      list(as.name("neighbourhood")), ngroups))
    makests(observed = as.data.frame(counts),
            populationFrac = c(popCH),
            neighbourhood = neighbourhood)
  } else { # list of by = "regions" sts objects by age group
    sapply(dimnames(counts)[[3L]], function(g){
      map$POPULATION <- popCH[, g]
      makests(observed = counts[, , g],
              populationFrac = prop.table(popCH[, g]),
              map = map,
              neighbourhood = neighbourhood)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
}