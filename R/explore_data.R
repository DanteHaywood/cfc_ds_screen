# ------------------------------------------------------------------------------
# explore_data.R
# Perform basic data exploration and validation.
# ------------------------------------------------------------------------------

library(tidyverse)


# Read image created in init.R
load("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")

# Summary Statistics -----------------------------------------------------------

dim(food_desert)

# Check for duplicates on keys. Output should have 0 rows if no dupes.

check_dupes(food_desert, "CensusTract")



# Summary statistics on interesting variables

# Replace NA and any outliers

# Join datasets

# Create interesting summary statistics for presentation







