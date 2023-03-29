redistribute_vaccines <- function(data_set){
  total_doses_given <- dcast(data_set[, c("dateISO", "altersklasse_covid19", "entries")],
                             dateISO ~ altersklasse_covid19, fun.aggregate = sum)
  doses_given_age <- do.call(
    rbind, lapply(X = seq_len(dim(total_doses_given)[1]),
                  FUN = function(x){round(colSums(popCH) / sum(popCH) *
                                            sum(total_doses_given[x, which(names(total_doses_given) != "dateISO")]))
                    # round since cannot give part dose
                  }))
  rownames(doses_given_age) <- total_doses_given$dateISO
  ## Replace entries
  data_set$entries <- sapply(
    seq_len(dim(data_set)[1]),
    function(x){
      return(
        doses_given_age[which(rownames(doses_given_age) == data_set[x, ]$dateISO),
                        which(colnames(doses_given_age) == data_set[x, ]$altersklasse_covid19)]
      )
    }
  )
  # Does not sum to same amount
  if(sum(data_set$entries) != sum(total_doses_given[, which(names(total_doses_given) != "dateISO")])){
    adj_factor <- sum(total_doses_given[, which(names(total_doses_given) != "dateISO")]) / sum(data_set$entries)
    data_set$entries <- data_set$entries * adj_factor
  }
  return(data_set)
}