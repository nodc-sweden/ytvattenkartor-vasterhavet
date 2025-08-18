library(R.matlab)
library(dplyr)
library(purrr)
library(tibble)

# Load the parameter mapping from a separate R script
source(file.path("R", "load_data.R"))

# Read MATLAB .mat file containing statistical data
mat <- readMat("data/reference_data/stat_stations.mat")

# Extract the 3D array of station statistics
stat_array <- mat$stat

# Extract station names and re-encode them from Latin1 to UTF-8
station_names <- sapply(mat$alla.stat, function(x) iconv(x[[1]], from = "latin1", to = "UTF-8"))

# Convert 3D array into a tidy tibble:
stats_tidy <- map_dfr(seq_len(dim(stat_array)[3]), function(station_index) {
  station_data <- stat_array[,,station_index]
  keep_rows <- apply(station_data, 1, function(r) !all(is.na(r)))
  station_data <- station_data[keep_rows, , drop = FALSE]
  param_id <- station_data[,1]
  depth <- station_data[,2]

  # Loop over 12 months and extract statistics
  map_dfr(1:12, function(month) {
    col_start <- 2 + (month-1)*5 + 1
    vals <- station_data[, col_start:(col_start+4)]
    tibble(
      station = station_names[station_index],
      parameter_id = param_id,
      depth = depth,
      month = month,
      mean = vals[,1],
      std = vals[,2],
      `2std` = vals[,3],
      min = vals[,4],
      max = vals[,5]
    )
  })
}) %>% left_join(parameter_map, by = "parameter_id") %>%
  mutate(source = "MATLAB")

# Fix spacing issue in one station name (data error correction)
stats_tidy$station[stats_tidy$station == "L9  LAHOLMSBUKTEN"]   <- "L9 LAHOLMSBUKTEN"