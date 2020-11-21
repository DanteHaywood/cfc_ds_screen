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

# density_est_2016
# Confirm no divide by zero
sum(data_join$population_estimate_2016 == 0)

data_join$log_density_est_2016 <- log(data_join$land_area_square_miles_2010 *
  # Divide by square feet in mile for conversion.
  # Capita per square mile is very small and incomprehensible
  27878400 / 
  data_join$population_estimate_2016
  )
hist(data_join$log_density_est_2016)

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


# Create training and test dataset ---------------------------------------------




