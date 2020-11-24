# ------------------------------------------------------------------------------
# init.R
# Install and load required packages, load the datasets, and save workspace.
# THIS SCRIPT IS REQUIRED TO REPRODUCE RESULTS.
# ------------------------------------------------------------------------------


# Install Req'd packages -------------------------------------------------------
packages <- c(
  "tidyverse",
  "randomForest",
  "glmnet",
  "caret",
  "ggmap",
  "pROC",
  "rgdal",
  "rgeos",
  "broom",
  "maptools",
  "mapproj",
  "viridis"
)


# Only install if needed.
#install.packages(packages)

# Avoid a bunch of library calls
# character.only needed to specify that we're using quotes for package names
lapply(packages, library, character.only = TRUE)

# Get utility Functions --------------------------------------------------------
# See this file for documentation if it is not clear where an object comes from.
source("R/utils.R")

# Load data --------------------------------------------------------------------
# Filepath is relative to project, not init.R.
# Datasets have first column as index, which is why we see a warning after load.
food_desert <- read_csv("data/food.desert.csv")
pop <- read_csv("data/population.csv")
ruca <- read_csv("data/ruca.usda.csv")
stores <- read_csv("data/stores.csv")
vehicles <- read_csv("data/vehicles.csv")

# Convert column names to lower case.
food_desert <- cols_to_lower(food_desert)
pop <- cols_to_lower(pop)
ruca <- cols_to_lower(ruca)
stores <- cols_to_lower(stores)
vehicles <- cols_to_lower(vehicles)


# Set Seed  --------------------------------------------------------------------
r_seed = 20201125
set.seed(r_seed)


# Save workspace to load in later scripts. -------------------------------------
#save.image("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace_bkup.RData")
save.image("~/GitHub/cfc_ds_screen/cfc_ds_screen_workspace.RData")
