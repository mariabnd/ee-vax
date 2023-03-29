opts_chunk$set(fig.height = 4, # Fixed height of figures
               echo = FALSE, # Do not include code
               warning = FALSE,
               message = FALSE,
               cache = FALSE, # Do not save work
               eval = TRUE, # Run the code
               fig.align = "center")
options(warn = 1) # Stop warning turning into error
par(las = 1) # For visualisation - axis label rotation
# Set default colour options to use viridis
options(ggplot2.continuous.colour = "viridis",
        ggplot2.continuous.fill = "viridis")
scale_colour_discrete <- function(...){
  scale_colour_manual(..., values = viridis(9))
}
scale_fill_discrete <- function(...){
  scale_fill_manual(..., values = viridis(9))
}
# For harmonisation purposes such that all plots look similar
theme_set(theme_base(base_size = 16) +
            # Match base R in look for consistency with other plots
            theme(plot.title = element_text(hjust = 0.5)) +
            # Centre titles
            theme(plot.background = element_blank()) +
            # Remove frame
            theme(text = element_text(family = "sans", size = 8)) +
            # Sans serif font
            theme(axis.ticks.length = unit(0.1, "cm")) +
            # Bold legend title
            theme(legend.title = element_text(face = "bold")))
# Ensure tables have consistant formatting
options(knitr.table.format = "latex", # LaTeX since we use .Rnw
        # Include missings as empty text string
        knitr.kable.NA = "",
        knitr.table.NA = "",
        width = 80,
        digits = 3,
        linesep = "",
        booktabs = TRUE,
        escape = FALSE)
# Define study period
date_range <- as.Date(c("2021-01-01", "2021-11-30"))
## As ISO weeks
date_range_ISO <- c(paste(isoWeekYear(date_range)$ISOYear[1],
                          isoWeekYear(date_range)$ISOWeek[1], sep = "-"),
                    paste(isoWeekYear(date_range)$ISOYear[2],
                          isoWeekYear(date_range)$ISOWeek[2], sep = "-"))
# Extract all ISO weeks in the study period to pad dates where needed,
# e.g. for vaccination coverage before vaccines rolled out
date_range_full <- unique(
  paste(isoWeekYear(seq.Date(date_range[1], date_range[2], by = 1))$ISOYear,
        isoWeekYear(seq.Date(date_range[1], date_range[2], by = 1))$ISOWeek,
        sep = "-"))
# Locations we are NOT using
# Lichtenstein and Switzerland total
region_exclude <- c("all", "CH", "CHFL", "FL", "neighboring_chfl", "unknown",
                    "CH01", "CH02", "CH03", "CH04", "CH05", "CH06", "CH07",
                    "<NA>")
# Age groups we are NOT using
age_exclude <- c("0 - 9", "5 - 11")
set.seed(20221015) # For reproducibility e.g. random effects in EE models

tp <- length(date_range_full) - 1