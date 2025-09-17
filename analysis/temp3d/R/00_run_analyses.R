library(coldpool)
library(navmaps)
library(spmodel)

# Get data and make sample size table
source("./R/01_get_akfin_data.R")
source("./R/90_sample_size_table.R")

# Create GOA layers - This takes ~16 hours
survey_definition_id <- 47
source("./R/02_compare_models.R")
source("./R/03_model_selection_prediction.R")
source("./R/04_temperature_maps.R")

# Create AI layers - This takes 8 hours
survey_definition_id <- 52
source("./R/02_fit_models.R")
source("./R/02_compare_models.R")
source("./R/03_model_selection_prediction.R")
source("./R/04_temperature_maps.R")

# Make plots and tables
