library(tidyverse)
library(sf)
library(ggrepel)

# Load helper functions
source("code/helper.R")

year <- 2025
month <- 1
data_file <- "data/BH_19_21_2025-05-27.txt"

parameter_of_interest <- "Temp CTD (prio CTD)"
depth_of_interest <- 0  # e.g., surface water

statistics <- R.matlab::readMat("data/stat_stations.mat")
coast <- R.matlab::readMat("data/map_info/bvvf_coast.dat")

# Load stats
# 1. Station names (fix encoding)
station_names <- sapply(statistics$alla.stat, function(x) iconv(x[[1]], from = "latin1", to = "UTF-8"))

# 2. Numeric array [210×62×23]
stat_array <- statistics$stat

# 3. Build tidy_stat, but first drop all-NaN rows in each station slice
tidy_stat <- map_dfr(seq_len(dim(stat_array)[3]), function(station_index) {
  # extract station slice
  station_data <- stat_array[,,station_index]
  
  # identify rows that are NOT *all* NaN:
  keep_rows <- apply(station_data, 1, function(r) !all(is.na(r)))
  station_data <- station_data[keep_rows, , drop = FALSE]
  
  # pull IDs
  param_id <- station_data[,1]
  depth    <- station_data[,2]
  
  # stack months
  map_dfr(1:12, function(month) {
    # columns: 1=id, 2=depth, then for month m: cols (2 + (m-1)*5 + 1):(2 + (m-1)*5 + 5)
    col_start <- 2 + (month-1)*5 + 1
    vals      <- station_data[, col_start:(col_start+4)]
    
    tibble(
      station      = station_names[station_index],
      parameter_id = param_id,
      depth        = depth,
      month        = month,
      mean         = vals[,1],
      std          = vals[,2],
      `2std`       = vals[,3],
      min          = vals[,4],
      max          = vals[,5]
    )
  })
})

parameter_map <- tibble::tibble(
  parameter_id = 1:10,
  parameter_name = c(
    "Temp CTD (prio CTD)",         # 1 = temperature
    "Salt CTD (prio CTD)",         # 2 = salinity
    "O2_CTD (prio CTD)",           # 3 = oxygen
    "O2Sat CTD (calc and prio-mix-max CTD)", # 4 = oxygen saturation
    "H2S",                          # 5
    "PO4",                          # 6
    "DIN",                          # 7 ← derived, not originally in your file
    "SiO4",                         # 8
    "Chla",                         # 9
    "Secchi"                        # 10
  ),
  parameter_name_short =c(
    "Temperatur",         # 1 = temperature
    "Salt",         # 2 = salinity
    "Syre",           # 3 = oxygen
    "Syremättnad", # 4 = oxygen saturation
    "H2S",                          # 5
    "Fosfat",                          # 6
    "DIN",                          # 7 ← derived, not originally in your file
    "Kisel",                         # 8
    "Chla",                         # 9
    "Secchi"                        # 10
))

parameter_short <- parameter_map$parameter_name_short[parameter_map$parameter_name == parameter_of_interest]

tidy_stat <- tidy_stat %>%
  left_join(parameter_map, by = "parameter_id")

# Remove space
tidy_stat$station[tidy_stat$station == "L9  LAHOLMSBUKTEN"]   <- "L9 LAHOLMSBUKTEN"

# Load data
data <- read_tsv(data_file, locale = locale(encoding = "ISO-8859-1"), col_types = cols())

# Standardize specific station names
# data$Station[data$Station == "BJÖRKHOLMEN"]   <- "INRE GULLMARN/BJÖRKHOLM"
# data$Station[data$Station == "STRETUDDEN"]    <- "BROFJORDEN/STRETUDDEN"
data$Station[data$Station == "KOSTERFJORDEN"] <- "KOSTERFJORDEN (NR16)"

# Calculate DIN
data <- data %>%
  mutate(
    NO2_fix = if_else(is.na(NO2), 0, NO2),
    NO3_fix = if_else(is.na(NO3), 0, NO3),
    NH4_fix = if_else(is.na(NH4), 0, NH4),
    DIN_raw = NO2_fix + NO3_fix + NH4_fix,
    DIN = if_else(DIN_raw == 0, NA_real_, DIN_raw)
  ) %>%
  select(-NO2_fix, -NO3_fix, -NH4_fix, -DIN_raw)  # Optional: clean up

data <- data %>%
  filter(Year == year) %>%
  filter(`Month (calc)` == month)


data$lat <- convert_dmm_to_dd(as.numeric(data$Lat))
data$lon <- convert_dmm_to_dd(as.numeric(data$Lon))


# Compare with historical values
data_joined <- data %>%
  filter(!is.na(`Temp CTD (prio CTD)`)) %>%
  mutate(
    Station = toupper(Station),
    Month = as.integer(`Month (calc)`),
    depth = as.integer(Depth)
  ) %>%
  left_join(
    tidy_stat %>%
      filter(parameter_name == parameter_of_interest, depth == depth_of_interest),
    by = c("Station" = "station", "Month" = "month")
  ) %>%
  mutate(
    pie_fill = mapply(
      assign_pie_fill,
      value = `Temp CTD (prio CTD)`,
      mean = mean,
      std = std,
      two_std = `2std`
    )
  )

data_joined <- data_joined %>%
  mutate(
    anomaly_flag = case_when(
      is.na(`Temp CTD (prio CTD)`) ~ "missing",
      `Temp CTD (prio CTD)` < min ~ "below_min",
      `Temp CTD (prio CTD)` > max ~ "above_max",
      TRUE ~ "normal"
    )
  )

data_joined <- data %>%
  filter(!is.na(.data[[parameter_of_interest]])) %>%
  filter(Depth == depth_of_interest) %>%
  mutate(
    Station = toupper(Station),
    Month = as.integer(`Month (calc)`),
    depth = as.integer(Depth)
  ) %>%
  left_join(
    tidy_stat %>%
      filter(parameter_name == parameter_of_interest, depth == depth_of_interest),
    by = c("Station" = "station", "Month" = "month")
  ) %>%
  mutate(
    pie_fill = mapply(
      assign_pie_fill,
      value = .data[[parameter_of_interest]],
      mean = mean,
      std = std,
      two_std = `2std`
    )
  ) %>%
  mutate(
    value = .data[[parameter_of_interest]]
  )


#### Plotting

points_sf <- st_as_sf(data_joined, coords = c("lon", "lat"), crs = 4326)

bbox <- st_bbox(points_sf)
xpad <- 0.2
ypad <- 0.2

# Load coast data
sw_coast <- st_read("data/EEA_Coastline_Polygon_Shape_Swedish_west_coast/Swedish_West_Coast_WGS84.shp")

# Read the file (assuming it's tab-separated and has no header)
border <- read_delim("data/map_info/gransen.txt", delim = "\t", col_names = FALSE)

# Rename columns
colnames(border) <- c("lon", "lat")

# Read the river file
rivers <- read_delim("data/map_info/elver.txt", delim = "\t", col_names = FALSE)

# Rename columns
colnames(rivers) <- c("lon", "lat")

# Define the fill colors per category
anomaly_colors_swe <- c(
  "Mycket högre än normalt" = "#d73027",
  "Högre än normalt"        = "#fdae61",
  "Normala värden"          = "#91bfdb",
  "Lägre än normalt"        = "#4575b4",
  "Mycket lägre än normalt" = "#313695",
  "Ingen provtagning"       = "grey70"
)

# Define all possible anomaly levels
all_anomalies <- c(
  "Mycket högre än normalt",
  "Högre än normalt",
  "Normala värden",
  "Lägre än normalt",
  "Mycket lägre än normalt",
  "Ingen provtagning"
)

month_names_sv <- c("Januari", "Februari", "Mars", "April", "Maj", "Juni",
                    "Juli", "Augusti", "September", "Oktober", "November", "December")

# Prepare plotting data
plot_data <- data_joined %>%
  mutate(
    anomaly_swe = case_when(
      value < mean - `2std` ~ "Mycket lägre än normalt",
      value >= mean - `2std` & value < mean - std ~ "Lägre än normalt",
      value >= mean - std & value <= mean + std ~ "Normala värden",
      value > mean + std & value <= mean + `2std` ~ "Högre än normalt",
      value > mean + `2std` ~ "Mycket högre än normalt",
      is.na(value) ~ "Ingen provtagning"
    ),
    anomaly_swe = factor(anomaly_swe, levels = all_anomalies),
    extreme = factor(case_when(
      value < min ~ "Över/under max/min",
      value > max ~ "Över/under max/min",
      TRUE        ~ "Inom historiskt intervall"
    ), levels = c("Inom historiskt intervall", "Över/under max/min")),
    combined_label = paste0(Station, "\n", round(value, 2))
  )

# Add dummy rows for missing anomaly_swe levels to ensure full legend
missing_levels <- setdiff(all_anomalies, unique(plot_data$anomaly_swe))

dummy_points <- tibble::tibble(
  lon = NA,
  lat = NA,
  anomaly_swe = factor(missing_levels, levels = all_anomalies),
  extreme = factor("Inom historiskt intervall", levels = c("Inom historiskt intervall", "Över/under max/min")),
  combined_label = NA
)

# Add dummy rows for both extreme levels to complete the color legend
dummy_extreme_points <- tibble::tibble(
  lon = NA,
  lat = NA,
  anomaly_swe = factor("Normala värden", levels = all_anomalies),  # any valid level
  extreme = factor(c("Inom historiskt intervall", "Över/under max/min"),
                   levels = c("Inom historiskt intervall", "Över/under max/min")),
  combined_label = NA
)

# Combine with real data
plot_data_complete <- bind_rows(plot_data, dummy_points, dummy_extreme_points)

# Create the plot
plot <- ggplot() +
  geom_sf(data = sw_coast, fill = "#eeeac4", color = "darkgrey", linewidth = .1) +
  geom_path(data = rivers,
            aes(x = lon, y = lat),
            color = "black",
            linewidth = 0.1) +
  geom_path(data = border,
            aes(x = lon, y = lat),
            color = "black",
            linewidth = 0.1,
            linetype = "dashed") +
  geom_point(
    data = plot_data_complete,
    aes(x = lon, y = lat, fill = anomaly_swe, color = extreme),
    shape = 21, size = 4, stroke = 0.7, na.rm = TRUE
  ) +
  scale_fill_manual(
    values = anomaly_colors_swe
  ) +
  scale_color_manual(
    values = c(
      "Inom historiskt intervall" = "black",
      "Över/under max/min" = "red"
    )
  ) +
  ggrepel::geom_text_repel(
    data = filter(plot_data_complete, !is.na(combined_label)),
    aes(x = lon, y = lat, label = combined_label),
    size = 2,
    fontface = "bold"
  ) +
  # coord_sf() +
  coord_sf(
    xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad),
    ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad),
    expand = FALSE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    fill = "Avvikelse från normalvärde",
    color = "Extremvärde",
    title = parameter_short,
    subtitle = paste(month_names_sv[month], year),
    x = "Longitud", y = "Latitud"
  ) +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  )

ggsave(paste0("plots/", parameter_short, "_", month, "_", year, ".png"),
       plot,
       bg = "white"
       )
