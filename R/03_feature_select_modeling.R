# feature_select_modeling ------------------------------------------------------
# Feature selection and modeling.

source("R/init.R")

# Univariate Logistic Regression Feature Selection -----------------------------
# Remove dependent variables from numerics
numerics <- numerics[numerics %in% c("food_desert_2017", "food_desert_2010",
                                     "select_state_county_tract") == FALSE]
all_predictors <- c(numerics, "primary_ruca_code_2010")
  
uni_logis_df <- uni_logis(train, "food_desert_2017", 
                          independents = all_predictors)
sum(uni_logis_df$converged == FALSE)
# 15 variables have pvalue under 0.05, which means we have some decent choices
sum(uni_logis_df$pvalue <= 0.05)

ggplot(uni_logis_df, aes(pvalue, reorder(variable, -pvalue))) +
  geom_col() +
  scale_x_continuous(breaks = seq(0,1,0.05))

uni_logi_vars <- uni_logis_df[uni_logis_df$pvalue <= 0.1, "variable"]

# Random Forest Classification Feature Selection -------------------------------
# Add the food desert factor variable - needed for randomForest classification

all_train$food_desert_2017_f <- factor(all_train$food_desert_2017)
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
  geom_col() + 
  labs(title = "Random Forest (k = 50) Feature Importance", 
       subtitle = "(Larger is more important)") +
  xlab("Importance (Mean Decrease Gini)") +
  ylab("Variable") +
  theme(text = element_text(color = "#22211d", size = 14),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47")
  )

probas_test_rf <- predict(selected_rf, test, type = "prob")[,2]

# Use the smooth parameter to control the number of points on graph.
# This will allow plotting different roc calculations together.
rf_roc <- roc(test$food_desert_2017, probas_test_rf,
              smooth = TRUE, smooth.n = 200)
roc_coords_rf <- data.frame(tpr = rf_roc$sensitivities,
                            fpr = 1 - rf_roc$specificities)

ggplot(roc_coords_rf, aes(fpr, tpr)) +
  geom_line(color = "blue", size = 0.8, show.legend = TRUE) +
  geom_abline(intercept = 0, color = "red", linetype = "longdash", 
              show.legend = TRUE) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme(legend.position = c(.95, .95))

# Logistic Regression ----------------------------------------------------------


selected_f <- as.formula(paste("food_desert_2017", " ~ ", 
                               paste(selected_features, collapse = " + "))
)

logi1 <- glm(selected_f, family = binomial, data=train)
summary(logi1)

# Remove some variables since they are not significant. COuld use stepwise
# to eliminate manual changes.
insigs <- c("pct_hhold_2plus_veh", "log_pop_per_sqmi_est_2018")
selected_features <- selected_features[selected_features %in% insigs == FALSE]
selected_f <- as.formula(paste("food_desert_2017", " ~ ", 
                               paste(selected_features, collapse = " + "))
)
logi2 <- glm(selected_f, family = binomial, data=train)
summary(logi2)

probas_test_logi <- predict(logi2, test, type = "response")

logi_roc <- roc(test$food_desert_2017, probas_test_logi, 
                smooth = TRUE, smooth.n = 200)
roc_coords_logi <- data.frame(tpr = logi_roc$sensitivities,
                            fpr = 1 - logi_roc$specificities)

ggplot(roc_coords_logi, aes(fpr, tpr)) +
  geom_line(color = "blue", size = 0.8, show.legend = TRUE) +
  geom_abline(intercept = 0, color = "red", linetype = "longdash") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme(legend.position = c(.95, .95))

# The random forest performs slightly better  >5% ROC AUC
# This could be due to more variables in Random Forest and non-linearity
# with log-odds.

# Final Model Evaluation -------------------------------------------------------

# Plot ROC curves next to each other

roc_coords_all <- data.frame(tpr_rf = roc_coords_rf$tpr,  
                             fpr_rf = roc_coords_rf$fpr,
                             tpr_logi = roc_coords_logi$tpr,  
                             fpr_logi = roc_coords_logi$fpr)
rf_legend_text <- paste("Random Forest (k = 50):", 
                        as.character(round(rf_roc$auc, 2)))
logi_legend_text <- paste("Logistic Regression:", 
                        as.character(round(logi_roc$auc, 2)))
rand_legend_text <- "Random"

ggplot(roc_coords_all) +
  geom_line(aes(x = fpr_rf, y = tpr_rf, color = "rf_line"), size = 1.2) +
  geom_line(aes(x = fpr_logi, y = tpr_logi, color = "logi_line"),  size = 1.2) +
  geom_abline(aes(intercept = 0, slope = 1, color = "ref_line"), 
              linetype = "longdash", size = 1, show.legend = FALSE) +
  labs(title = "NC Food Desert Prediction", subtitle = "Model ROC") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  scale_colour_manual(name="ROC AUC",
                      values = c(rf_line = "blue", logi_line="orange",
                                 ref_line = "red"),
                      labels = c(rf_line = rf_legend_text, 
                                 logi_line = logi_legend_text, 
                                 ref_line = rand_legend_text)
                      ) +
  theme(legend.position = c(0.80, 0.15),
        text = element_text(color = "#22211d", size = 14),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47")
        )

# Store Model ------------------------------------------------------------------
probas_final <- predict(selected_rf, all_train, type = "prob")[,2]
preds_final <- predict(selected_rf, all_train)
all_train$probas_final <- probas_final
all_train$preds_final <- preds_final
hist(all_train$probas_final)
# Sanity check: ROC AUC should be super high
final_roc <- roc(all_train$food_desert_2017, probas_final, 
                smooth = TRUE, smooth.n = 200)

save.image("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")

#