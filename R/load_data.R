library(tibble)
library(readr)
library(readxl)

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
  parameter_unit = c(
    "°C", "psu", "ml/l", "%", "H2S", "µmol/l", 
    "µmol/l", "µmol/l", "µg/l", "m"
  ),
  parameter_depth = c(rep(" i ytvattnet", 2), " i bottenvattnet", rep(" i ytvattnet", 6), ""),
  shark_colname = c(
    "Temperature CTD (C)",
    "Salinity CTD (o/oo psu)",
    "Dissolved oxygen O2 CTD (ml/l)",
    "O2_saturation",
    "Hydrogen sulphide H2S (umol/l)",
    "Phosphate PO4-P (umol/l)",
    "DIN",
    "Silicate SiO3-Si (umol/l)",
    "Chlorophyll-a bottle (ug/l)",
    "Secchi depth (m)"
  )
)

# Define all Swedish anomaly labels (used for categorizing measurement deviation)
all_anomalies <- c(
  "Mycket högre än normalt", "Högre än normalt", "Normala värden",
  "Lägre än normalt", "Mycket lägre än normalt", "Saknar referensvärde"
)

# Define color codes for each anomaly level (used in maps and plots)
anomaly_colors_swe <- c(
  "Mycket högre än normalt" = "#440154FF",
  "Högre än normalt" = "#3B528BFF",
  "Normala värden" = "#35b779",
  "Lägre än normalt" = "#b5de2b",
  "Mycket lägre än normalt" = "#FDE725FF",
  "Saknar referensvärde" = "white"
)

# Swedish month names (used for dropdowns, labels, etc.)
month_names_sv <- c("januari", "februari", "mars", "april", "maj", "juni",
                    "juli", "augusti", "september", "oktober", "november", "december")

# Stations to downloaded from SHARK
station_names <- read_lines(file.path("data", "config", "station_names.txt"))

# Stations to downloaded from SHARK
platform_codes <- read_lines(file.path("data", "config", "platform_codes.txt"))

# Download to a temporary file
url <- "https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx"
tmp <- tempfile(fileext = ".xlsx")

# This will be used as a fallback if the download or read fails.
empty_df <- tibble(
  Data_field = character(),
  Code = character(),
  `Beskrivning/Svensk översättning` = character()
)

# Try to download and read the SMHI Codelist
smhi_codelist <- tryCatch({
  download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
  # Read columns A–C starting from row 2 (skip header row).
  df <- read_xlsx(tmp, range = cell_limits(c(2, 1), c(NA, 3))) %>%
    filter(Data_field == "SHIPC")
}, error = function(e) {
  empty_df
})

# Ensure that all platform_codes are present in the output,
shipc_codes <- tibble(Code = platform_codes) %>%
  left_join(
    smhi_codelist %>% select(Code, `Beskrivning/Svensk översättning`),
    by = "Code"
  )

# Build checkbox labels.
shipc_codes$label <- ifelse(
  is.na(shipc_codes$`Beskrivning/Svensk översättning`),
  shipc_codes$Code,
  paste0(shipc_codes$Code, " – ", shipc_codes$`Beskrivning/Svensk översättning`)
)
