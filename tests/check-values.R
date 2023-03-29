library(tinytest)
# Check entire study period covered by data
expect_true(all(movement_mats >= 0),
            info = "Check values -- adjacency")

expect_true(all(contact_mats >= 0),
            info = "Check values -- adjacency")
