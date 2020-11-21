# feature_gen_multivariate------------------------------------------------------
# Feature generation and multivariate analysis.

load("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")


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

# pop_1k_per_sqmi_est_2018
# 24 tracts with apparent population of 0
sum(data_join$population2018 <= 0)
sum(data_join$population2018 < 0)
hist(data_join$population2018)
# Meanwhile, the RUCA codes do not agree that the population is zero.
# This is not an issue given the population is an estimate, but still should
# consider.
sum(data_join$population2018 == 0 & data_join$primary_ruca_code_2010 == 99)


# 1,000 people per square mile in tract
# Add 1 to avoid zero
data_join$pop_1k_per_sqmi_est_2018 <- (data_join$population2018 / 1000 + 1) / 
                                  (data_join$land_area_square_miles_2010 + 1)
hist(data_join$pop_1k_per_sqmi_est_2018)
summary(data_join$pop_1k_per_sqmi_est_2018)

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

non_pred_vars <- c("state_county_fips_code",
             "select_county",
             "select_state_county_tract",
             
             )

 

# Create training and test dataset ---------------------------------------------



















#
save.image("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")

