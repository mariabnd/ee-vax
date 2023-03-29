# CONTACTS
school <- read.csv("data/Switzerland_country_level_F_school_setting_85.csv",
                   header = FALSE)
work <- read.csv("data/Switzerland_country_level_F_work_setting_85.csv",
                 header = FALSE)
household <- read.csv(
  "data/Switzerland_country_level_F_household_setting_85.csv",
  header = FALSE)
other <- read.csv("data/Switzerland_country_level_F_community_setting_85.csv",
                  header = FALSE)

m_household <- as.matrix(household)
m_school <- as.matrix(school)
m_work <- as.matrix(work)
m_other <- as.matrix(other)

orig <- ncol(m_other)
lim <- as.numeric(strsplit(" - ", x = age_exclude)[[1]][2])
keep <- which(as.numeric(gsub("V", "", colnames(m_household))) > lim)
rm(lim)
m_household <- m_household[keep, keep]
m_school <- m_school[keep, keep]
m_work <- m_work[keep, keep]
m_other <- m_other[keep, keep]

# Disease weights
mistry <- c("school" = 11.41, "work" = 8.07,
            "household" = 4.11, "other" = 2.79)

cont <- m_school * mistry["school"] +
  m_work * mistry["work"] +
  m_household * mistry["household"] +
  m_other * mistry["other"]
# This creates the labels for the contact matrices such that it is clear what
# age group the corresponding row/column represents.
# The ifelse statement ensures that the labels follow the convention that the
# minimum and maximum age have two digits and follow the format xx-xx
## Create list of numbers sequentially from 00 to 84
lower <- ifelse(nchar(0 : (orig - 1)) == 1,
                        paste0(0, 0 : (orig - 1)),
                        (0 : (orig - 1)))
## Paste with copy of itself plus one 
upper <- as.numeric(lower) + 1
upper <- ifelse(nchar(upper) == 1,
       paste0(0, upper),
       (upper))
## and add dash between the two
row_col_names <- paste(lower, upper, sep = "-")
rm(lower); rm(upper)

## Add plus at very end
#row_col_names[length(row_col_names)] <-
#  gsub("-.+", "\\+", row_col_names[length(row_col_names)])

row_col_names <- row_col_names[keep]

# Note the difference between how contact matrices usually shown (POLYMOD) and
# how they enter endemic-epidemic models (Meyer and Held 2017)
dim_names <- list(participant = row_col_names,
                  contact = row_col_names)

dimnames(cont) <- dim_names
grp <- c(10, 10, 10, 10, 10, 10, 10,
         dim(cont)[2] - 70)
Cgrouped <- aggregateC(cont, grouping = c(10, 10, 10, 10, 10, 10, 10,
                                          dim(cont)[2] - 70))

colnames(Cgrouped) <- rownames(Cgrouped) <- c("10 - 19", "20 - 29",
                                              "30 - 39", "40 - 49", "50 - 59",
                                              "60 - 69", "70 - 79", "80+")
rm(orig); rm(keep)
contact_for_plot <- Cgrouped
# Row-normalise as in Meyer and Held (2017)
Cgrouped <- Cgrouped / rowSums(Cgrouped)

# POLICY
# https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/OxCGRT_latest.csv
data_poli <- read.csv("data/OxCGRT_latest.csv")
data_poli <- data_poli[data_poli$CountryCode == "CHE", ]

data_poli$Date <- as.Date(as.character(data_poli$Date), format = "%Y%m%d")
# ISO week
data_poli$dateISO <-
  paste(isoWeekYear(data_poli$Date)$ISOYear,
        ifelse(nchar(as.numeric(isoWeekYear(data_poli$Date)$ISOWeek) + 1) == 1,
               # if one digit add leading zero
               paste0("0", as.numeric(isoWeekYear(data_poli$Date)$ISOWeek)),
               as.numeric(isoWeekYear(data_poli$Date)$ISOWeek)), sep = "-")
data_poli$dateISO <- fixISOleadingzero(data_poli$dateISO)
# Restrict to study period
data_poli <- data_poli[data_poli$dateISO %in% date_range_full, ]

# Average over ISO week to obtain the weekly amount
data_poli <- merge(
  aggregate(C1_School.closing ~ dateISO, data = data_poli, FUN = mean),
  merge(
    aggregate(C2_Workplace.closing ~ dateISO, data = data_poli, FUN = mean),
    merge(
      aggregate(C3_Cancel.public.events ~ dateISO, data = data_poli, FUN = mean),
      merge(
        aggregate(C4_Restrictions.on.gatherings ~ dateISO,
                  data = data_poli, FUN = mean),
        merge(
          aggregate(C5_Close.public.transport ~ dateISO,
                    data = data_poli, FUN = mean),
          merge(
            aggregate(C6_Stay.at.home.requirements ~ dateISO,
                      data = data_poli, FUN = mean),
            merge(
              aggregate(C7_Restrictions.on.internal.movement ~ dateISO,
                        data = data_poli, FUN = mean),
              merge(aggregate(C8_International.travel.controls ~ dateISO,
                        data = data_poli, FUN = mean),
              aggregate(StringencyIndex ~ dateISO,
                        data = data_poli, FUN = mean)))))))))

# C5 and C7 all take the value 0 for the original study dates
# so are not considered
data_poli$C1_change <- policy_norm(data_poli$C1_School.closing, max = 3)
data_poli$C2_change <- policy_norm(data_poli$C2_Workplace.closing, max = 3)
data_poli$C3_change <- policy_norm(data_poli$C3_Cancel.public.events, max = 2)
data_poli$C4_change <- policy_norm(data_poli$C4_Restrictions.on.gatherings, max = 4)
data_poli$C6_change <- 1#policy_norm(data_poli$C6_Stay.at.home.requirements, max = 3, rev = FALSE)
data_poli$Index_normalised <- data_poli$StringencyIndex / 100
# Lower bound of 0.001
data_poli$C1_change <- ifelse(data_poli$C1_change == 0,
                                    data_poli$C1_change + 0.001,
                                    data_poli$C1_change)
data_poli$C2_change <- ifelse(data_poli$C2_change == 0,
                              data_poli$C2_change + 0.001,
                              data_poli$C2_change)
#data_poli$C6_change <- ifelse(data_poli$C6_change == 0,
#                              data_poli$C6_change + 0.001,
#                              data_poli$C6_change)
data_poli$change_other <- data_poli$C3_change * data_poli$C4_change
data_poli$change_other <- ifelse(data_poli$change_other == 0,
                              data_poli$change_other + 0.001,
                              data_poli$change_other)
# Ensure correct ordering
data_poli <- data_poli[order(data_poli$dateISO), ]

# Array with contacts
contact_mats <- array(data = NA,
                      dim = c(dim(Cgrouped),
                              length(date_range_full)),
                      dimnames = list(unique(rownames(Cgrouped)),
                                      unique(rownames(Cgrouped)),
                                      date_range_full))

for (i in seq_len(dim(contact_mats)[3])){
  df <- data_poli[data_poli$dateISO == dimnames(contact_mats)[[3]][i], ]
  adj <- 
    unique(data_mobi[data_mobi$dateISO == dimnames(contact_mats)[[3]][i],
                     c("dateISO", "avg_movement_change")])$avg_movement_change
  tmp <- m_school * mistry["school"] * df$C1_change +
    m_work * mistry["work"] * df$C2_change +
    m_household * mistry["household"] * df$C6_change +
    m_other * mistry["other"] * df$C4_change #df$change_other
  tmp <- tmp * adj
  colnames(tmp) <- colnames(cont)
  rownames(tmp) <- rownames(cont)
  tmp <- aggregateC(tmp, grouping = grp)
  colnames(tmp) <- rownames(tmp) <- rownames(Cgrouped)
  tmp <- as.matrix(tmp)
  ## Row-normalise as in Meyer and Held (2017)
  #tmp <- tmp / rowSums(tmp)
  contact_mats[, , i] <- tmp
}

# Save with regional
load(file = "output/matrices.Rdata")
save(movement_mats, contact_mats,
     file = "output/matrices.Rdata")

rm(cont, m_household, m_school, m_other, m_work,
   household, school, other, work)
rm(tmp, df, row_col_names, mistry)
rm(grp)