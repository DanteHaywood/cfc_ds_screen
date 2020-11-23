# utils.R ----------------------------------------------------------------------
# Utility functions and info.

# check_dupes-------------------------------------------------------------------
# Checks if duplicates exist in dataframe.
# Arguments:
#   df: A dataframe or tibble. Expects rows as observations.
#   col: A single column on which to check for duplicate values.
# Value:
#   list:
#     any_dupes: BOOL. If there are any duplicates found.
#     dupes: Tibble. Any duplicate rows on column.
# Tested: TRUE

check_dupes <- function(df, column) {
  require(dplyr)
  
  dupes <- df %>%
    group_by(.data[[column]]) %>%
    filter(n() > 1) %>%
    arrange(.data[[column]])
    
  
  n_dupes <- dim(dupes)[1]
  
  list(
    "any_dupes" = n_dupes > 0,
    "dupes" = dupes
  )
}

# cols_to_lower ----------------------------------------------------------------
# Converts column names to lower case.
# Arguments:
#   df: A dataframe or tibble.
# Value:
#   df: The dataframe with column names set to lowercase.
# NOTE: Does not check for duplicate names after lowercasing.
# Tested: TRUE

cols_to_lower <- function(df) {
  require(dplyr)
  
  df %>% rename_all(tolower)
  
}

# impute_values ----------------------------------------------------------------
# Imputes values with given replacer. Can impute NA, NaN, Inf, char,
# and numerics.
# Arguments:
#   x: A column vector,
# Value:
#   x : An object the same length as x with imputed values.
# Tested: FALSE

impute_values <- function(x, value_to_replace = NA, replacer = 0) {
  
  # NA
  if (is.na(value_to_replace)) {
    
    replace_idx <- is.na(x)
    
  # NaN
  } else if (is.nan(value_to_replace)) {
    
    replace_idx <- is.nan(x)
    
  # Inf, -Inf
  } else if (is.infinite(value_to_replace)) {
    
    replace_idx <- is.infinite(x)
    
  # Value
  } else {
    
    replace_idx <- x[x == value_to_replace]
    
  }
  
  x[replace_idx] <- replacer
  
  return(x)
  
}


# uni_logis --------------------------------------------------------------------
# Runs a set of univariate logistic regressions and returns useful fit info.
# and numerics.
# Arguments:
#   df: A dataframe with the desired variables.
#   dependent: A character variable for y.
#   independents: A character vector of independent, predictor variables.
# Value:
#   all_model_df : A dataframe:
#     variable: The independent variable for a given univariate model fit.
#     coeff: The fit coefficient for the independent variable.
#     aic: The AIC for the fit.
#     converged: Binary if the model converged.
#     pvalue: Independent variable test of significance's p-value.
# Tested: TRUE
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

