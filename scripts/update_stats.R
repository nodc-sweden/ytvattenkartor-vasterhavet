library(SHARK4R)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

source(file.path("R", "load_data.R"))

# Define year interval
to_year = 2024
from_year <- to_year-9

# Stations to download
station_names <- read_lines(file.path("data", "map_info", "station_names.txt"))

# Download Physchem data from SHARK
shark_data <- get_shark_data(fromYear = from_year, 
                             toYear = to_year,
                             tableView = "sharkdata_physicalchemical_columns",
                             dataTypes = "Physical and Chemical",
                             stationName = station_names,
                             verbose = FALSE)

# Rename specific stations for consistency with plotting/statistical datasets
shark_data$station_name[shark_data$station_name == "KOSTERFJORDEN NR16"] <- "KOSTERFJORDEN (NR16)"

# Store year ranges for each station in a data frame
years_df <- shark_data %>%
  group_by(station_name) %>%
  summarise(
    year_min = min(visit_year, na.rm = TRUE),
    year_max = max(visit_year, na.rm = TRUE)
  )

# Find available year
available_max_year <- max(shark_data$visit_year)
available_min_year <- min(shark_data$visit_year)

# Calculate DIN
shark_data <- shark_data %>%
  mutate(
    NO2_fix = if_else(is.na(`Nitrite NO2-N (umol/l)`), 0, `Nitrite NO2-N (umol/l)`),
    NO3_fix = if_else(is.na(`Nitrate NO3-N (umol/l)`), 0, `Nitrate NO3-N (umol/l)`),
    NH4_fix = if_else(is.na(`Ammonium NH4-N (umol/l)`), 0, `Ammonium NH4-N (umol/l)`),
    DIN_raw = NO2_fix + NO3_fix + NH4_fix,
    DIN = if_else(DIN_raw == 0, NA_real_, DIN_raw)
  ) %>%
  select(-NO2_fix, -NO3_fix, -NH4_fix, -DIN_raw)

# Remove syremättnad for now as it does not exist in SHARK
parameter_map <- parameter_map %>%
  filter(!parameter_name_short == "Syremättnad")

# Standard depths
standard_depth <- c(
  0, 2, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, 90, 100, 120, 140, 180, 220, 250
)

depth_lim <- data.frame(
  lower = c(0, 1.2, 4, 7, 12, 17, 22, 27, 32, 37, 47, 55, 65, 75, 85, 95, 110, 130, 160, 200, 230),
  upper = c(1.2, 4, 7, 12, 17, 22, 27, 32, 37, 47, 55, 65, 75, 85, 95, 110, 130, 160, 200, 230, 250)
)

# Only use parameter columns that exist in shark_data
param_cols <- intersect(parameter_map$shark_colname, names(shark_data))

# Clean numeric columns (handle ".", ",", "<0.1", etc.)
shark_clean <- shark_data %>%
  select(
    station = station_name,
    depth   = sample_depth_m,
    month   = visit_month,
    all_of(param_cols)
  ) %>%
  mutate(
    across(
      all_of(param_cols),
      ~ {
        x <- as.character(.x)
        x[x %in% c(".", "", "NA")] <- NA
        readr::parse_number(str_replace_all(x, ",", "."))
      }
    )
  )

# Define the function to map depth to nearest standard depth
depth_lookup <- function(depth) {
  if (is.na(depth)) return(NA_real_)
  standard_depth[which.min(abs(standard_depth - depth))]
}

# Apply without rowwise()
shark_clean <- shark_clean %>%
  mutate(std_depth = sapply(depth, depth_lookup))

# Pivot and calculate stats
stats <- shark_clean %>%
  pivot_longer(
    cols = all_of(param_cols),
    names_to = "parameter_name",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  left_join(
    parameter_map %>% select(parameter_id, shark_colname),
    by = c("parameter_name" = "shark_colname")
  ) %>%
  filter(!is.na(parameter_id), !is.na(std_depth), !is.na(month)) %>%
  group_by(station, parameter_id, std_depth, month) %>%
  summarise(
    n     = sum(!is.na(value)),
    mean  = ifelse(n > 2, mean(value, na.rm = TRUE), NA_real_),
    std   = ifelse(n > 2, sd(value, na.rm = TRUE), NA_real_),
    `2std`= ifelse(n > 2, 2 * sd(value, na.rm = TRUE), NA_real_),
    min   = ifelse(n > 2, min(value, na.rm = TRUE), NA_real_),
    max   = ifelse(n > 2, max(value, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  # Drop entire depth/param rows where all 12 months are NA
  group_by(station, parameter_id, std_depth) %>%
  filter(!all(is.na(mean))) %>%
  ungroup() %>%
  arrange(station, parameter_id, std_depth, month) %>%
  rename(depth = std_depth) %>% 
  select(-n) %>%
  left_join(parameter_map, by = "parameter_id") %>%
  left_join(years_df, by = c("station" = "station_name"))

# Append to list
stats_list[[as.character(paste0(available_min_year, "-", available_max_year))]] <- stats

# Sort alphabetically by names
stats_list <- stats_list[order(names(stats_list))]

# Save data
saveRDS(stats_list, file.path("data", "reference_data", "reference_data.rds"))

# Create output folder
out_dir <- file.path("data", "reference_data", "txt")
dir.create(out_dir, showWarnings = FALSE)

# Loop and write each data frame to a separate text file
purrr::walk2(
  stats_list,
  names(stats_list),
  ~ write_tsv(.x, file.path(out_dir, paste0(.y, ".txt")), progress = FALSE)
)
