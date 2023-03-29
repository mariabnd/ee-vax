#' Extract information about model fit
#'
#' @description Step one extract estimate and add SE in brackets
#' Step two combines (cbind) these estimates for the models considered
#' @param mod_list A list of hhh4 models to extract information from
#' @param se_bracket Boolean determining whether to include the standard error
#' in the cell as "X (SE Y)" (TRUE) or whether to return the estimate X and
#' standard error Y as two separate columns (FALSE). Defaults to TRUE
#' @param include_SE A Boolean which determines whether to annotate the standard
#' error by SE. Defaults to TRUE
#' @returns a data frame with the estimated effects for each of the models
#' included in the list of models
#' @seealso coef_for_table
#' @example 
#' # Using the examples from hhh4
#' data("measlesWeserEms")
#' mod1 <- hhh4(measlesWeserEms,
#' list(
#' end = list(f = addSeason2formula(~1 + t, period = 52),
#' offset = population(measlesWeserEms)),
#' ne = list(f = ~1, weights = neighbourhood(measlesWeserEms) == 1),
#' family = "NegBin1"
#' ))
#' mod2 <- update(mod1, ne = list(f = addSeason2formula(f = ~ 1)))
#' model_fit_info(list("model 1" = mod1, "model 2" = mod2))
#'                        model 1               model 2
#' ne.1                   "-1.456 (SE 0.179)" "-3.354 (SE 0.605)"
#' end.1                  "0.627 (SE 0.345)"  "0.487 (SE 0.34)"  
#' end.t                  "0.006 (SE 0.006)"  "0.013 (SE 0.006)" 
#' end.sin(2 * pi * t/52) "0.82 (SE 0.269)"   "1.301 (SE 0.226)" 
#' end.cos(2 * pi * t/52) "0.85 (SE 0.196)"   "1.091 (SE 0.183)" 
#' overdisp               "11.361 (SE 0.995)" "10.656 (SE 0.951)"
#' ne.sin(2 * pi * t/52)  NA                  "1.027 (SE 0.421)" 
#' ne.cos(2 * pi * t/52)  NA                  "-2.151 (SE 0.69)" 
model_fit_info <- function(mod_list, se_bracket = TRUE, include_SE = TRUE){
  if(class(mod_list) != "list"){
    stop("mod_list must be a list")
  }
  if(unique(sapply(mod_list, class)) != "hhh4"){
    stop("mod_list must contain hhh4 models")
  }
  if(!is.logical(se_bracket)){
    stop("se_bracket must be TRUE/FALSE")
  }
  mod_df <- lapply(mod_list, function(mod){
    if(se_bracket){
    df <- coef_for_table(mod, unvax_focus = FALSE, include_SE = include_SE)
    }
    if(!se_bracket){
      df <- coefficients(mod, se = TRUE)
    }
    return(as.data.frame(t(df)))
  })
  df <- t(do.call(plyr::rbind.fill, mod_df))
  idx <- lapply(mod_list, function(mod){
    has_ranefs <- !is.null(summary(mod)$ranef)
    if(has_ranefs){
      out <- names(ranef(mod))
    } else {
      out <- NULL
    }
    return(out)
  })
  idx <- unique(do.call(rbind, idx))
  df <- df[!(rownames(df) %in% idx), ]
  if(se_bracket){
    colnames(df) <- names(mod_list)
  }
  if(!se_bracket){
    colnames(df) <- paste0(c("est-", "se-"), rep(names(mod_list), each = 2))
  }
  return(df)
}
