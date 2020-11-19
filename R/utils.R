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
#   LIST:
#     any_dupes: BOOL. If there are any duplicates found.
#     dupes: Tibble. Any duplicate rows on column.
# ------------------------------------------------------------------------------
check_dupes <- function(df, column) {
  
  dupes <- df %>%
    group_by(.data[[column]]) %>%
    filter(n() > 1) 
  
  n_dupes <- dim(dupes)[1]
  
  list(
    "any_dupes" = n_dupes > 0,
    "dupes" = dupes
  )
}


