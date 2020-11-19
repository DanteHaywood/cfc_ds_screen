# ------------------------------------------------------------------------------
# utils.R
# Utility functions and info.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# check_dupes
# Checks if duplicates exist in dataframe.
# INPUTS:
#   df: A dataframe or tibble. Expects rows as observations.
#   col: A single column on which to check for duplicate values.
# OUTPUTS:
#   list:
#     any_dupes: BOOL. If there are any duplicates found.
#     dupes: Tibble. Any duplicate rows on column.
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# cols_to_lower
# Converts column names to lower case.
# INPUTS:
#   df: A dataframe or tibble.
# OUTPUTS:
#   df: The dataframe with column names set to lowercase.
# NOTE: Does not check for duplicate names after lowercasing.
# ------------------------------------------------------------------------------
cols_to_lower <- function(df) {
  require(dplyr)
  
  df %>% rename_all(tolower)
  
}


