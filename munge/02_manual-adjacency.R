## hard-coded adjacency orders to avoid re-calculation (and "spdep" dependency)
# Note that NE and JU are not considered to share a border
# Note that SH and TG are not considered to share a border
# Perhaps useful to have co-authors also calculate the adjancies and
# where discrepancy exists solve by consensus
map_nbOrder <- structure(
  c(# From AG
    0L, 3L, 3L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 2L, 1L, 2L,
    2L, 2L, 2L, 2L, 1L, 2L, 2L, 3L, 2L, 2L, 2L, 1L, 1L,
    # From AI
    3L, 0L, 1L, 4L, 4L, 5L, 5L, 6L, 2L, 2L, 5L, 3L, 5L,
    3L, 4L, 1L, 2L, 4L, 2L, 2L, 3L, 3L, 5L, 4L, 3L, 2L,
    # From AR
    3L, 1L, 0L, 4L, 4L, 5L, 5L, 6L, 2L, 2L, 5L, 3L, 5L,
    3L, 4L, 1L, 2L, 4L, 2L, 2L, 3L, 3L, 5L, 4L, 3L, 2L,
    # From BE
    1L, 4L, 4L, 0L, 2L, 3L, 1L, 2L, 2L, 2L, 1L, 1L, 1L,
    1L, 1L, 3L, 3L, 1L, 2L, 3L, 2L, 1L, 1L, 1L, 2L, 2L,
    # From BL
    1L, 4L, 4L, 2L, 0L, 1L, 3L, 5L, 4L, 4L, 1L, 2L, 3L,
    3L, 3L, 3L, 3L, 1L, 3L, 3L, 4L, 3L, 4L, 3L, 2L, 2L,
    # From BS
    2L, 5L, 5L, 3L, 1L, 0L, 4L, 6L, 5L, 5L, 2L, 3L, 4L,
    4L, 4L, 4L, 4L, 2L, 4L, 4L, 5L, 4L, 5L, 4L, 3L, 3L,
    # From FR
    2L, 5L, 5L, 1L, 3L, 4L, 0L, 2L, 3L, 3L, 2L, 2L, 1L,
    2L, 2L, 4L, 4L, 2L, 3L, 4L, 3L, 2L, 1L, 2L, 3L, 3L,
    # From GE
    3L, 6L, 6L, 2L, 5L, 6L, 2L, 0L, 4L, 4L, 3L, 3L, 2L,
    3L, 3L, 5L, 5L, 3L, 4L, 5L, 3L, 3L, 1L, 2L, 4L, 4L,
    # From GL
    3L, 2L, 2L, 2L, 4L, 5L, 3L, 4L, 0L, 1L, 3L, 2L, 3L,
    2L, 2L, 1L, 3L, 3L, 1L, 2L, 2L, 1L, 3L, 2L, 2L, 2L,
    # From GR
    3L, 2L, 2L, 2L, 4L, 5L, 3L, 4L, 1L, 0L, 3L, 3L, 3L,
    2L, 2L, 1L, 3L, 3L, 2L, 2L, 1L, 1L, 3L, 2L, 3L, 2L,
    # From JU
    2L, 5L, 5L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 0L, 2L, 2L,
    2L, 2L, 4L, 4L, 1L, 3L, 4L, 3L, 2L, 2L, 2L, 3L, 3L,
    # From LU
    1L, 3L, 3L, 1L, 2L, 3L, 2L, 3L, 2L, 3L, 2L, 0L, 2L,
    1L, 1L, 2L, 3L, 2L, 1L, 3L, 3L, 2L, 2L, 2L, 1L, 2L,
    # From NE
    2L, 5L, 5L, 1L, 3L, 4L, 1L, 2L, 3L, 3L, 2L, 2L, 0L,
    2L, 2L, 4L, 4L, 2L, 3L, 4L, 3L, 2L, 1L, 2L, 3L, 3L,
    # From NW
    2L, 3L, 3L, 1L, 3L, 4L, 2L, 3L, 2L, 2L, 2L, 1L, 2L,
    0L, 1L, 2L, 3L, 2L, 1L, 3L, 2L, 1L, 2L, 2L, 2L, 2L,
    # From OW
    2L, 4L, 4L, 1L, 3L, 4L, 2L, 3L, 2L, 2L, 2L, 1L, 2L,
    1L, 0L, 3L, 4L, 2L, 2L, 4L, 2L, 1L, 2L, 2L, 2L, 3L,
    # From SG
    2L, 1L, 1L, 3L, 3L, 4L, 4L, 5L, 1L, 1L, 4L, 2L, 4L,
    2L, 3L, 0L, 2L, 3L, 1L, 1L, 2L, 2L, 4L, 3L, 2L, 1L,
    # From SH
    2L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 3L, 3L, 4L, 3L, 4L,
    3L, 4L, 2L, 0L, 3L, 2L, 2L, 4L, 3L, 4L, 4L, 2L, 1L,
    # From SO
    1L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 1L, 2L, 2L,
    2L, 2L, 3L, 3L, 0L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L,
    # From SZ
    2L, 2L, 2L, 2L, 3L, 4L, 3L, 4L, 1L, 2L, 3L, 1L, 3L,
    1L, 2L, 1L, 2L, 3L, 0L, 2L, 2L, 1L, 3L, 2L, 1L, 1L,
    # From TG
    2L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 2L, 2L, 4L, 3L, 4L,
    3L, 4L, 1L, 2L, 3L, 2L, 0L, 3L, 3L, 4L, 4L, 2L, 1L,
    # From TI
    3L, 3L, 3L, 2L, 4L, 5L, 3L, 3L, 2L, 1L, 3L, 3L, 3L,
    2L, 2L, 2L, 4L, 3L, 2L, 3L, 0L, 1L, 2L, 1L, 3L, 3L,
    # From UR
    2L, 3L, 3L, 1L, 3L, 4L, 2L, 3L, 1L, 1L, 2L, 2L, 2L,
    1L, 1L, 2L, 3L, 2L, 1L, 3L, 1L, 0L, 2L, 1L, 2L, 2L,
    # From VD
    2L, 5L, 5L, 1L, 4L, 5L, 1L, 1L, 3L, 3L, 2L, 2L, 1L,
    2L, 2L, 4L, 4L, 2L, 3L, 4L, 2L, 2L, 0L, 1L, 3L, 3L,
    # From VS
    2L, 4L, 4L, 1L, 3L, 4L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
    2L, 2L, 3L, 4L, 2L, 2L, 4L, 1L, 1L, 1L, 0L, 3L, 3L,
    # From ZG
    1L, 3L, 3L, 2L, 2L, 3L, 3L, 4L, 2L, 3L, 3L, 1L, 3L,
    2L, 2L, 2L, 2L, 2L, 1L, 2L, 3L, 2L, 3L, 3L, 0L, 1L,
    # From ZH
    1L, 2L, 2L, 2L, 2L, 3L, 3L, 4L, 2L, 2L, 3L, 2L, 3L,
    2L, 3L, 1L, 1L, 2L, 1L, 1L, 3L, 2L, 3L, 3L, 1L, 0L),
  .Dim = c(26L, 26L),
  .Dimnames = list(
    c("AG", "AI", "AR", "BE", "BL", "BS",
      "FR", "GE", "GL", "GR", "JU", "LU",
      "NE", "NW", "OW", "SG", "SH", "SO",
      "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
    c("AG", "AI", "AR", "BE", "BL", "BS",
      "FR", "GE", "GL", "GR", "JU", "LU",
      "NE", "NW", "OW", "SG", "SH", "SO",
      "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH")))
mobility_for_plot <- map_nbOrder
# Add one and invert as in Meyer and Held (2014)
map_nbOrder <- 1 / (map_nbOrder + 1) # Add 1
# Neighbouring regions order + 1 for the power law (o + 1)