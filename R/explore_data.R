# ------------------------------------------------------------------------------
# explore_data.R
# Perform basic data exploration and validation.
# ------------------------------------------------------------------------------

library(tidyverse)


# Read image created in init.R
load("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")

# Deduping & Join Prep ---------------------------------------------------------
# Review join keys for part (1) of assignment: a single clean df with one
# row per tract.

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
# 37 is FIPS code for NC
vehicles$fips_tract <- str_c("37", vehicles$county, vehicles$tract)
# Now check no duplicates on new variable (chr)
check_dupes(vehicles, "fips_tract")


# Summary Statistics -----------------------------------------------------------

# Number of NC counties: 100 
# Cross-checked with internet search, so none are missing from food_desert.
nc_counties <- unique(food_desert$county)
nc_counties
length(nc_counties)

# Number of NC Census Tracts: 2,192
# NC currently has 2,195 tracts according to census.gov:
# https://www.census.gov/geographies/reference-files/2010/geo/state-local-geo-guides-2010/north-carolina.html#:~:text=North%20Carolina%20has%202%2C195%20census,groups%2C%20and%20288%2C987%20census%20blocks.
nc_tracts <- unique(food_desert$censustract)
length(nc_tracts)

# Impute 0 for 2010 food desert since only NA and 1 unique values.
# NOTE: NA != 0. Did the USDA not evaluate these counties?
# ASSUMPTION: USDA evaluated all counties and NA counties are not food deserts.
unique(food_desert$food_desert_2010)

food_desert$food_desert_2010 <- impute_values(food_desert$food_desert_2010)

sum(food_desert$food_desert_2010)
sum(food_desert$food_desert_2017)




# Replace NA and any outliers

# Join datasets

# Create interesting summary statistics for presentation







