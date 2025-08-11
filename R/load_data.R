# Read version from DESCRIPTION
pkg_version <- read.dcf("DESCRIPTION", fields = "Version")[1]

# GitHub repo link
github_url <- "https://github.com/nodc-sweden/ytvattenkartor-vasterhavet"

# Define a tibble that maps parameter metadata (ID, full name, short Swedish name)
parameter_map <- tibble::tibble(
  parameter_id = 1:10,
  parameter_name = c(
    "Temp CTD (prio CTD)", "Salt CTD (prio CTD)", "O2_CTD (prio CTD)",
    "O2Sat CTD (calc and prio-mix-max CTD)", "H2S", "PO4", "DIN",
    "SiO4", "Chla", "Secchi"
  ),
  parameter_name_short = c(
    "Temperatur", "Salt", "Syre", "Syremättnad", "H2S", "Fosfat", 
    "DIN", "Kisel", "Chla", "Secchi"
  ),
  parameter_name_plot = c(
    "Temperatur", "Salthalt", "Syrgaskoncentration", "Syremättnad", "H2S", "Fosfatkoncentration", 
    "DIN-koncentration", "Kiselkoncentration", "Klorofyllkoncentration", "Secchidjup"
  ),
  parameter_depth = c(rep(" i ytvattnet", 2), " i bottenvattnet", rep(" i ytvattnet", 6), "")
)

# Define all Swedish anomaly labels (used for categorizing measurement deviation)
all_anomalies <- c(
  "Mycket högre än normalt", "Högre än normalt", "Normala värden",
  "Lägre än normalt", "Mycket lägre än normalt", "Saknar historiska värden"
)

# Define color codes for each anomaly level (used in maps and plots)
anomaly_colors_swe <- c(
  "Mycket högre än normalt" = "#d73027",
  "Högre än normalt" = "#fdae61",
  "Normala värden" = "#66bd63",
  "Lägre än normalt" = "#91bfdb",
  "Mycket lägre än normalt" = "#313695",
  "Saknar historiska värden" = "white"
)

# Swedish month names (used for dropdowns, labels, etc.)
month_names_sv <- c("januari", "februari", "mars", "april", "maj", "juni",
                    "juli", "augusti", "september", "oktober", "november", "december")

# Read MATLAB .mat file containing statistical data
mat <- readMat("data/stat_stations.mat")

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
}) %>% left_join(parameter_map, by = "parameter_id")

# Fix spacing issue in one station name (data error correction)
stats_tidy$station[stats_tidy$station == "L9  LAHOLMSBUKTEN"]   <- "L9 LAHOLMSBUKTEN"