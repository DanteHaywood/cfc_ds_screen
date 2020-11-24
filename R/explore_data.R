# explore_data.R ---------------------------------------------------------------
# Perform basic data exploration and validation.

source("R/init.R")


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
# RUCA dataset actually contains 2,195 tracts, so will be using that as base
# for joining.
nc_tracts <- unique(food_desert$censustract)
length(nc_tracts)

# Impute 0 for 2010 food desert since only NA and 1 unique values.
# NOTE: NA != 0. Did the USDA not evaluate these counties?
# ASSUMPTION: USDA evaluated all counties and NA counties are not food deserts.
unique(food_desert$food_desert_2010)

food_desert$food_desert_2010 <- impute_values(food_desert$food_desert_2010)

# NC tracts listed as food deserts 2010: 134
sum(food_desert$food_desert_2010)
# NC tracts listed as food deserts 2017: 368
# Why the increase?
sum(food_desert$food_desert_2017)

# Summarize location of food_desert data by county
unique(food_desert$povertyrate)
food_desert_summary_county <- food_desert %>% 
  group_by(county) %>%
  summarise(n_tract = n(),
            mean_poverty_rate_tract = mean(povertyrate),
            mean_family_income_tract = mean(medianfamilyincome),
            median_poverty_rate_tract = median(povertyrate),
            median_family_income_tract = median(medianfamilyincome),
            n_food_desert_tract_2010 = sum(food_desert_2010),
            n_food_desert_tract_2017 = sum(food_desert_2017),
            # Normmalize over number of tracts in county
            pct_food_desert_tract_2010 = sum(food_desert_2010) / n(),
            pct_food_desert_tract_2017 = sum(food_desert_2017) / n(),
            chg_food_desert_2010_2017 = sum(food_desert_2017) - 
              sum(food_desert_2010)
  ) %>%
  arrange(desc(mean_poverty_rate_tract))

food_desert_summary_county

# Barplot across each variable in summary to find cutoffs.

# Poverty Rate
# Mean poverty rate cutoff determined 20
# No real OUtliers here with an outlandish poverty rate in comparison to others.
ggplot(food_desert_summary_county, 
       aes(mean_poverty_rate_tract, 
           reorder(county, mean_poverty_rate_tract))) +
  geom_col()


top20_county_mean_poverty <- slice_max(food_desert_summary_county, 
                               mean_poverty_rate_tract, n=20)$county
bar_idx <- food_desert$county %in% top20_county_mean_poverty


ggplot(food_desert[bar_idx,], aes(county, povertyrate)) + 
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 60))

# Median Family Income
# Several wealthy counties: Orange and Wake
# Poorest family income is Hyde by ~$15K to next highest
ggplot(food_desert_summary_county, 
       aes(mean_family_income_tract, 
           reorder(county, mean_family_income_tract))) +
  geom_col()

# Food desert 2010
# 54/100 of counties did not have USDA designated food desert in 2010 eval
sum(food_desert_summary_county$pct_food_desert_tract_2010 == 0)
ggplot(food_desert_summary_county, 
       aes(pct_food_desert_tract_2010, 
           reorder(county, pct_food_desert_tract_2010))) +
  geom_col()

# Food desert 2017
# 83/100 counties with at least one food desert county
# Scotland, Hyde, and Bladen with highest percents of food deserts
sum(food_desert_summary_county$pct_food_desert_tract_2017 > 0)
ggplot(food_desert_summary_county, 
       aes(pct_food_desert_tract_2017, 
           reorder(county, pct_food_desert_tract_2017))) +
  geom_col()

# Change in food desert status between 2010 and 2017
# Counties that removed food desert tracts: 4
sum(food_desert_summary_county$chg_food_desert_2010_2017 < 0)
# Counties that saw no change in food desert tracts: 24
sum(food_desert_summary_county$chg_food_desert_2010_2017 == 0)
# Counties that had a larger number of food desert tracts in 2017: 72
sum(food_desert_summary_county$chg_food_desert_2010_2017 > 0)

ggplot(food_desert_summary_county, 
       aes(reorder(county, chg_food_desert_2010_2017), 
       chg_food_desert_2010_2017)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 60))


# Join datasets
# Join pop to ruca first since pop only has county names without FIPS code.
# This is a workaround to match the FIPS code with county names.
# Typically I would use a lookup table to do this since any mistakes in 
# either dataset regarding names and FIPS codes will propagate.
# RUCA also has the correct number of tracts, 2,195.

pop[c("x1", "state")] <- list(NULL)
ruca[c("x1","select_state")] <- list(NULL)


data_join <- left_join(ruca, pop, by = "county")
# Confirm no dupes
dim(data_join)
dim(ruca)

food_desert[c("x1","state")] <- list(NULL)

# Use subset to avoid adding duplicate variables/names
data_join <- data_join %>% left_join(subset(food_desert, select = -county), 
                        by = c("select_state_county_tract" = "censustract"))
dim(data_join)
# See which tracts are missing food desert data
data_join[is.na(data_join$food_desert_2017),]

# Stores county variable does not have county appended to column so adding.
stores$county <- str_c(str_trim(stores$county, side = "both"), " County")
stores$x1 <- NULL
data_join <- data_join %>% left_join(stores, 
                                     by = c("select_county" = "county"))

vehicles[c("x1", "county", "tract")] <- NULL
# Convert to compatible type
vehicles$fips_tract <- as.numeric(vehicles$fips_tract)
data_join <- data_join %>% left_join(vehicles, 
                                     by = c("select_state_county_tract" = 
                                              "fips_tract"))
# Still maintain 2,195 unique tracts
dim(data_join)
check_dupes(data_join, "select_state_county_tract")

summary(data_join)

# From summary, 31 tracts are missing vehicle count data
# Likely, if these variables prove predictive, I will remove these tracts from
# modeling dataset. Since there are already zeros present, I cannot consider
# NA ~ 0. The impact of these 31 tracts recoded to 0 may not have much effect,
# however.
min(data_join$pct_hhold_1_veh, na.rm = TRUE)

# data_join should now satisfy the requirement for assignment (1):
# "Join data sets to create a clean df with one row per tract."

save.image("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")

