# feature_select_modeling ------------------------------------------------------
# Feature selection and modeling.


load("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")
lapply(packages, library, character.only = TRUE)

do.call(glm, list(as.formula("food_desert_2017", "~", paste(c("medianfamilyincome"), collapse = " + ")),
                  data = all_train, family=binomial)
        )

model <- glm(as.formula(paste("food_desert_2017", "~", "medianfamilyincome", collapse = " + ")),
  #formula = eval(call(quote("food_desert_2017"), "~", quote("food_desert_2017" + "medianfamilyincome"))),
             family = binomial,
             data = all_train)


uni_logis <- function(df, dependent, independents){

  for (var in independents) {
    f <- as.formula(paste(dependent, "~", var))
    cat("Model:", var, "\n")
    model <- glm(formula = f, 
                family = binomial,
                data = df)
    
    model_df <- data.frame(
      variable = var,
      coeff = as.numeric(model$coefficients[2]),
      aic = model$aic,
      converged = model$converged,
      pvalue = as.numeric(coef(summary(model))[,4][2])
    )
    
    if (var != independents[1]) {
      all_model_df <- rbind(all_model_df, model_df)
    } else {
      all_model_df <- model_df
    }

  }
  
  cat("Complete!")
  return(all_model_df)
  
}

# Remove dependent variables from numerics
numerics <- numerics[numerics %in% c("food_desert_2017", "food_desert_2010",
                                     "select_state_county_tract") == 
                       FALSE]
  
uni_logis_df <- uni_logis(train, "food_desert_2017", independents = numerics)
sum(uni_logis_df$converged == FALSE)
# 15 variables have pvalue under 0.05, which means we have some decent choices
sum(uni_logis_df$pvalue <= 0.05)

ggplot(uni_logis_df, aes(pvalue, reorder(variable, -pvalue))) +
  geom_col() +
  scale_x_continuous(breaks = seq(0,1,0.05))








#