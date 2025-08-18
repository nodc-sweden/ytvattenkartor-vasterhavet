library(SHARK4R)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

source(file.path("R", "load_data.R"))
source(file.path("R", "helper.R"))

# Read the reference data (stats_list) from the RDS file
stats_list <- readRDS(file.path("data", "reference_data", "reference_data.rds"))

# Define year interval
to_year = as.integer(format(Sys.Date(), "%Y")) - 1 # Previous calendar year, or a specific year, e.g. 2016
time_range <- 10                                   # 10 year interval
min_n <- 3

# Update the stats using the helper function
stats_list <- update_stats(to_year, time_range, stats_list, station_names, parameter_map, min_n, platform_codes)

# Create output folder
out_dir <- file.path("data", "reference_data", "txt")
dir.create(out_dir, showWarnings = FALSE)

# Loop and write each data frame to a separate text file (for viewing)
purrr::walk2(
  stats_list,
  names(stats_list),
  ~ write_tsv(.x, file.path(out_dir, paste0(.y, ".txt")), progress = FALSE)
)

# Alternatively, plot the stats as time series
# source(file.path("scripts", "plot_stats.R"))
