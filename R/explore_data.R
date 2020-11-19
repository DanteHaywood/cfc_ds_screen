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
# censustract is the full FIPS key (num)
check_dupes(food_desert, "censustract")

# Population only available by county
# county is FIPS code with 0 padding (chr)
check_dupes(pop, "county")

# select_state_county_tract is unique and full FIPS code for tract
check_dupes(ruca, "select_state_county_tract")

# Stores only available by county, so expect many-to-one join on tracts
check_dupes(stores, "county")

# dupes in vehicles by tract since it is not full FIPS
check_dupes(vehicles, "tract")
# Create full FIPS tract code
vehicles$fips_tract <- str_c("37", vehicles$county, vehicles$tract)
# Now check no duplicates on new variable (chr)
check_dupes(vehicles, "fips_tract")


# Summary statistics on interesting variables


# Replace NA and any outliers

# Join datasets

# Create interesting summary statistics for presentation







