# This script is a standalone, offline alternative to the
# interactive Shiny application for generating monthly
# environmental monitoring maps and reports from
# InfoC-export .txt data files.

# Load required packages
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(sf)
library(ggrepel)
library(png)
library(jpeg)
library(tiff)
library(grid)
library(gridExtra)
library(ggpubr)

# Load helper functions
source("R/helper.R")
source("R/load_data.R")

# ==== CONFIGURATION ====
data_file <- "data/BH_19_21_2025-05-27.txt"   # Input file (InfoC-export .txt)
output_dir <- "output"                   # Output folder for PNGs/PDF
plots_dir <- file.path(output_dir, "plots")
year <- 2025
month <- 5
reference_data <- "2007-2016"
bbox_option <- "Bohuslän och Halland"    # "Bohuslän", "Halland", ...
plot_width <- 15                         # cm
plot_height <- 20                        # cm
include_logo_smhi <- TRUE
include_logo_bvvf <- TRUE
include_logo_lans <- TRUE
add_shapes <- FALSE
only_flanks <- FALSE

# Create output directory
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# ==== LOAD AND PREPARE DATA ====
data_upload <- read_tsv(
  data_file,
  locale = locale(encoding = "ISO-8859-1"),
  col_types = cols(),
  progress = FALSE,
  name_repair = "unique_quiet"
)

# Fix station names
data_upload$Station[data_upload$Station == "KOSTERFJORDEN"] <- "KOSTERFJORDEN (NR16)"
data_upload$Station[data_upload$Station == "N7 OST NIDINGEN SLA 0-10"] <- "N7 OST NIDINGEN"

# Ensure month column is numeric
data_upload$`Month (calc)` <- as.numeric(data_upload$`Month (calc)`)

# Calculate DIN
data_upload <- data_upload %>%
  mutate(
    NO2_fix = if_else(is.na(NO2), 0, NO2),
    NO3_fix = if_else(is.na(NO3), 0, NO3),
    NH4_fix = if_else(is.na(NH4), 0, NH4),
    DIN_raw = NO2_fix + NO3_fix + NH4_fix,
    DIN = if_else(DIN_raw == 0, NA_real_, DIN_raw)
  ) %>%
  select(-NO2_fix, -NO3_fix, -NH4_fix, -DIN_raw)

# Convert coordinates
data_upload$lat <- convert_dmm_to_dd(as.numeric(data_upload$Lat))
data_upload$lon <- convert_dmm_to_dd(as.numeric(data_upload$Lon))

# Select reference dataset
stats_tidy <- stats_list[[reference_data]]

# ==== GENERATE PNG PLOTS FOR ALL PARAMETERS ====
png_files <- c()
for (param in parameter_map$parameter_name) {
  if (param == "H2S") next
  
  param_short <- parameter_map$parameter_name_short[parameter_map$parameter_name == param]
  file_path <- file.path(plots_dir, paste0(make.names(gsub("ä", "", param_short)),
                                           "_", year, "_", month, ".png"))
  
  file_saved <- save_param_plot(
    param = param,
    year = year,
    month = month,
    data = data_upload,
    stats_tidy = stats_tidy,
    all_anomalies = all_anomalies,
    anomaly_colors_swe = anomaly_colors_swe,
    month_names_sv = month_names_sv,
    parameter_map = parameter_map,
    bbox_option = bbox_option,
    plot_width = plot_width,
    plot_height = plot_height,
    out_path = file_path,
    add_shapes = add_shapes,
    reference_data = reference_data, 
    only_flanks = only_flanks
  )
  
  if (!is.null(file_saved)) png_files <- c(png_files, file_saved)
}

message(length(png_files), " PNG plots saved to ", output_dir)

# ==== GENERATE PDF MONTHLY REPORT ====
plots <- list()

# Logo/title page
logo_page <- create_logo_page(
  include_smhi = include_logo_smhi,
  include_bvvf = include_logo_bvvf,
  include_lans = include_logo_lans,
  month = month,
  year = year,
  month_names_sv = month_names_sv
)
plots[[1]] <- logo_page

# Parameter plots
for (param in parameter_map$parameter_name) {
  if (param == "H2S") next
  
  df_filtered <- data_upload %>%
    filter(Year == year, `Month (calc)` == month)
  
  if (nrow(df_filtered) == 0 || all(is.na(df_filtered[[param]]))) next
  
  p <- create_plot_for_param(
    param = param,
    year = year,
    month = month,
    data = data_upload,
    stats_tidy = stats_tidy,
    all_anomalies = all_anomalies,
    anomaly_colors_swe = anomaly_colors_swe,
    month_names_sv = month_names_sv,
    parameter_map = parameter_map,
    bbox_option = bbox_option,
    plot_width = plot_width,
    plot_height = plot_height,
    add_shapes = add_shapes, 
    reference_data = reference_data,
    only_flanks = only_flanks
  )
  
  if (!is.null(p)) plots[[length(plots) + 1]] <- p
}

pdf_file <- file.path(output_dir, paste0("plots_", year, "_", month, ".pdf"))

pdf(pdf_file, width = 8.27, height = 11.69, onefile = TRUE)
print(plots[[1]])
if (length(plots) > 1) {
  plot_width_in  <- plot_width / 2.54
  plot_height_in <- plot_height / 2.54
  for (i in seq(2, length(plots))) {
    grid.newpage()
    vp <- viewport(width = unit(plot_width_in, "in"),
                   height = unit(plot_height_in, "in"),
                   x = 0.5, y = 0.5, just = c("center", "center"))
    pushViewport(vp)
    print(plots[[i]], newpage = FALSE)
    popViewport()
  }
}
dev.off()

message("PDF monthly report saved to ", pdf_file)
