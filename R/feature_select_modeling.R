# feature_select_modeling ------------------------------------------------------
# Feature selection and modeling.


load("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")
lapply(packages, library, character.only = TRUE)


# Univariate Logistic Regression Feature Selection -----------------------------
# Remove dependent variables from numerics
numerics <- numerics[numerics %in% c("food_desert_2017", "food_desert_2010",
                                     "select_state_county_tract") == FALSE]
all_predictors <- c(numerics, "primary_ruca_code_2010")
  
uni_logis_df <- uni_logis(train, "food_desert_2017", independents = all_predictors)
sum(uni_logis_df$converged == FALSE)
# 15 variables have pvalue under 0.05, which means we have some decent choices
sum(uni_logis_df$pvalue <= 0.05)

ggplot(uni_logis_df, aes(pvalue, reorder(variable, -pvalue))) +
  geom_col() +
  scale_x_continuous(breaks = seq(0,1,0.05))

uni_logi_vars <- uni_logis_df[uni_logis_df$pvalue <= 0.1, "variable"]

# Random Forest Classification Feature Selection -------------------------------
# Add the food desert factor variable - needed for randomForest classification

train$food_desert_2017_f <- factor(train$food_desert_2017)
test$food_desert_2017_f <- factor(test$food_desert_2017)

# NOTE: Put into function
f <- as.formula(paste("food_desert_2017_f", " ~ ", 
                      paste(all_predictors, collapse = " + "))
                )
rf_selection <- randomForest(f, data = train, ntree = 50)
rf_importance <- data.frame(variable = row.names(rf_selection$importance),
                            importance = as.numeric(rf_selection$importance))

# The main difference between the two selection methods is that the RF
# places high importance on tract area. The univariate regressions did not
# pick this up. Likely this means there is some interactive effect between
# area and other variables since the RF is multivariate + non-linear.
ggplot(rf_importance, aes(importance, reorder(variable, importance))) +
  geom_col()

# Select Features  -------------------------------------------------------------
# Features are selected based on the two selection methods above.
# They are hand selected here to increase interpretability.
# For example, we do not want to include two population count variables.

selected_features <- c(
  "medianfamilyincome",
  "povertyrate",
  "land_area_square_miles_2010",
  "log_pop_per_sqmi_est_2018",
  "pct_hhold_2plus_veh",
  "pct_hhold_no_veh",
  "primary_ruca_code_2010",
  "convs14_per_1k_capita",
  "groc14_per_1k_capita",
  "mean_change_pop"
)

# Random Forest ----------------------------------------------------------------

# RF needs factor for classification.
selected_f <- as.formula(paste("food_desert_2017_f", " ~ ", 
                      paste(selected_features, collapse = " + "))
)

# Train and evaluate performance.
selected_rf <- randomForest(selected_f, data = train, ntree = 50)
rf_importance <- data.frame(variable = row.names(selected_rf$importance),
                            importance = as.numeric(selected_rf$importance))
ggplot(rf_importance, aes(importance, reorder(variable, importance))) +
  geom_col()

probas_test_rf <- predict(selected_rf, test, type = "prob")[,2]


rf_roc <- roc(test$food_desert_2017, probas_test_rf)
roc_coords_rf <- data.frame(tpr = rf_roc$sensitivities,
                            fpr = 1 - rf_roc$specificities, 
                            threshold = rf_roc$thresholds)

ggplot(roc_coords_rf, aes(fpr, tpr)) +
  geom_line(color = "blue", size = 0.8, show.legend = TRUE) +
  geom_abline(intercept = 0, color = "red", linetype = "longdash", show.legend = TRUE) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme(legend.position = c(.95, .95))

# Logistic Regression ----------------------------------------------------------


selected_f <- as.formula(paste("food_desert_2017_f", " ~ ", 
                               paste(selected_features, collapse = " + "))
)

logi1 <- glm(selected_f, family = binomial, data=train)
summary(logi1)

# Remove some variables since they are not significant. COuld use stepwise
# to elminate manual changes.
insigs <- c("pct_hhold_2plus_veh", "log_pop_per_sqmi_est_2018")
selected_features <- selected_features[selected_features %in% insigs == FALSE]
selected_f <- as.formula(paste("food_desert_2017", " ~ ", 
                               paste(selected_features, collapse = " + "))
)
logi2 <- glm(selected_f, family = binomial, data=train)
summary(logi2)

probas_test_logi <- predict(logi2, test, type = "response")


logi_roc <- roc(test$food_desert_2017, probas_test_logi)
roc_coords_logi <- data.frame(tpr = logi_roc$sensitivities,
                            fpr = 1 - logi_roc$specificities, 
                            threshold = logi_roc$thresholds)

ggplot(roc_coords_logi, aes(fpr, tpr)) +
  geom_line(color = "blue", size = 0.8, show.legend = TRUE) +
  geom_abline(intercept = 0, color = "red", linetype = "longdash", show.legend = TRUE) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme(legend.position = c(.95, .95))

# The random forest performs slightly better  >5% ROC AUC
# This could be due to more variables in Random Forest and non-linearity
# with log-odds.

# Final Model Selection --------------------------------------------------------



















#