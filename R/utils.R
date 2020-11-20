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


