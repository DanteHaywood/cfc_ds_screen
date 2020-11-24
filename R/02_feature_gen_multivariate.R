# feature_gen_multivariate------------------------------------------------------
# Feature generation and multivariate analysis.

source("R/init.R")

# Generate Features ------------------------------------------------------------

# mean_change_pop
# NOTE: more elegant way to do this?
mean_change_pop <- rowMeans(data.frame(
  data_join$population_estimate_2011 - data_join$x2010_census_population,
  data_join$population_estimate_2012 - data_join$population_estimate_2011,
  data_join$population_estimate_2013 - data_join$population_estimate_2012,
  data_join$population_estimate_2014 - data_join$population_estimate_2013,
  data_join$population_estimate_2015 - data_join$population_estimate_2014,
  data_join$population_estimate_2016 - data_join$population_estimate_2015
))
# COnfirm correct dim
length(mean_change_pop) == 2195
# Majority of tracts see increase in pop with some increasing rapidly.
hist(mean_change_pop)
data_join$mean_change_pop <- mean_change_pop

# pop_per_sqmi_est_2018
# 24 tracts with apparent population of 0
sum(data_join$population2018 <= 0)
sum(data_join$population2018 < 0)
hist(data_join$population2018)
# Meanwhile, the RUCA codes do not agree that the population is zero.
# This is not an issue given the population is an estimate, but still should
# consider.
sum(data_join$population2018 == 0 & data_join$primary_ruca_code_2010 == 99)


# People per square mile in tract
# Add 1 to avoid zero in log - this also recodes the 0 population tracts back
# to zero.
data_join$log_pop_per_sqmi_est_2018 <- log((data_join$population2018) / 
                            (data_join$land_area_square_miles_2010 + 1e-6) + 1)
                                  
hist(data_join$log_pop_per_sqmi_est_2018)
summary(data_join$log_pop_per_sqmi_est_2018)

data_join[1:10,c("land_area_square_miles_2010", "population2018","log_density_est_2018")]

# groc14_per_1k_capita
data_join$groc14_per_1k_capita <- data_join$groc14 / 
  (data_join$population_estimate_2014 / 1e3)
hist(data_join$groc14_per_1k_capita)

# superc14_per_1k_capita
data_join$superc14_per_1k_capita <- data_join$superc14 / 
  (data_join$population_estimate_2014 / 1e3)
hist(data_join$superc14_per_1k_capita)

# convs14_per_1k_capita
data_join$convs14_per_1k_capita <- data_join$convs14 / 
  (data_join$population_estimate_2014 / 1e3)
hist(data_join$convs14_per_1k_capita)

# specs14_per_1k_capita
data_join$specs14_per_1k_capita <- data_join$specs14 / 
  (data_join$population_estimate_2014 / 1e3)
hist(data_join$specs14_per_1k_capita)

# Multivariate Analysis --------------------------------------------------------
# Now with a set of possible predictor variables, check their interaction
# with food desert 2017.
data_join$tract_population_2010 - data_join$x2010_census_population
names(data_join)

# Remove non-predictor variables and linear dependence but keep tract id
non_pred_vars <- c("state_county_fips_code",
             "select_county",
             "county",
             "tract",
             "x2010_census_population",
             "population_estimate_2011",
             "population_estimate_2012",
             "population_estimate_2013",
             "population_estimate_2014",
             "population_estimate_2015",
             "population_estimate_2016",
             "pct_white_pop_2010",         
             "pct_black_pop_2010"
             )

# 14 variables to be removed out of 43,. so 29 left for model
length(non_pred_vars)
all_train <- data_join %>% select(!all_of(non_pred_vars))
names(all_train)
dim(all_train)

# Convert RUCA codes to factor to use in model correctly
all_train$primary_ruca_code_2010 <- factor(all_train$primary_ruca_code_2010)
all_train$secondary_ruca_code_2010 <- factor(all_train$secondary_ruca_code_2010)

names(all_train)

# Get list of numeric variables
numerics <- names(select_if(all_train, is.numeric))

# Plots help see distribution of two categories: food desert and not
# NOTES on below plots:
# population_estimate_2018: No real difference
# poverty_rate, medianfamilyincome: large indicators
# specs14: small indicator
# pct_hhold_no_veh + others: strong indicators
# mean_change_pop: small indicator
# log_pop_per_sqmi_est_2010: decent indicator
# Generally concerned that most variables do not have relationship with deserts

for (var in numerics) {
  print(
    ggplot(all_train, 
         aes(factor(food_desert_2017), .data[[var]])) +
      #geom_boxplot(aes(fill=factor(food_desert_2017)))
      geom_violin(aes(fill=factor(food_desert_2017)))
    )
}



# Deal with NA's ---------------------------------------------------------------
summary(all_train)

# Deal with NA's

# Remove 3 rows with food desert NA since they do not have a population
# estimate anyway.
summary(all_train[is.na(all_train$food_desert_2017),])

# Vehicle dataset has 31 NA
# Only 1 of these tracts has a population
summary(all_train[is.na(all_train$pct_hhold_1_veh),])
# Only 1 is listed as a food desert, so not removing our few tracts listed as
# food desert.
sum(all_train[is.na(all_train$pct_hhold_1_veh),'food_desert_2017'] == 1, 
    na.rm = TRUE)

# Removing all rows with NA seems justified with minimal loss of useful
# information.
# New dim: [2164, 29]
all_train <- all_train %>% na.omit() %>% arrange(select_state_county_tract)


# Create training and test dataset ---------------------------------------------
train <- all_train %>% sample_frac(.70)
test  <- anti_join(all_train, train, by = "select_state_county_tract")
dim(train)
dim(test)
# Confirm a reasonable split on dependent variable
sum(all_train$food_desert_2017) / dim(all_train)[1]
sum(train$food_desert_2017) / dim(train)[1]
sum(test$food_desert_2017)/ dim(test)[1]


save.image("cfc_ds_screen_workspace.RData")

