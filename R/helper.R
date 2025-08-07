# Conversion function
convert_dmm_to_dd <- function(dmm) {
  deg <- floor(dmm / 100)            # Extract degrees
  min <- dmm - deg * 100             # Extract minutes
  dd <- deg + min / 60               # Convert to decimal degrees
  return(dd)
}

# Function to assign pie fill level
assign_pie_fill <- function(value, mean, std, two_std) {
  if (is.na(value) || is.na(mean) || is.na(std) || is.na(two_std)) {
    return(NA_real_)
  } else if (value < mean - two_std) {
    return(0)
  } else if (value >= mean - two_std && value < mean - std) {
    return(0.25)
  } else if (value >= mean - std && value <= mean + std) {
    return(0.5)
  } else if (value > mean + std && value <= mean + two_std) {
    return(0.75)
  } else if (value > mean + two_std) {
    return(1)
  } else {
    return(NA_real_)
  }
}

has_garbled_utf8 <- function(x) {
  # Detect common mojibake patterns caused by UTF-8 misinterpreted as Latin1
  grepl("Ã.|Ã¥|Ã¤|Ã¶|Ã–|Ã„|Ã…|â|€|™", x)
}

# Function to create a plot
create_plot <- function(df, input, all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map) {
  # Load spatial data: Swedish west coast polygon and filtered lakes in Sweden
  sw_coast <- st_read("data/EEA_Coastline_Polygon_Shape_Swedish_west_coast/Swedish_West_Coast_WGS84.shp", quiet = TRUE)
  lakes <- st_read("data/ne_10m_lakes/sweden_lakes.shp", quiet = TRUE)
  
  # Load additional map features: country border and rivers
  border <- read_delim("data/map_info/gransen.txt", delim = "\t", col_names = c("lon", "lat"),
                       col_types = cols(), progress = FALSE)
  rivers <- read_delim("data/map_info/elver.txt", delim = "\t", col_names = c("lon", "lat"),
                       col_types = cols(), progress = FALSE)
  
  # Convert main data to spatial points for bounding box calculation
  df_points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  bbox <- st_bbox(df_points)
  xpad <- 0.2
  ypad <- 0.2
  
  # Override bbox based on user input
  if (input$bbox_option == "Bohuslän") {
    bbox <- st_bbox(c(xmin = 11.10333, ymin = 57.55000, xmax = 11.90667, ymax = 58.86833), crs = st_crs(4326))
  } else if (input$bbox_option == "Bohuslän och Halland") {
    bbox <- st_bbox(c(xmin = 11.10333, ymin = 56.56500, xmax = 12.72000, ymax = 58.86833), crs = st_crs(4326))
  } else if (input$bbox_option == "Halland") {
    bbox <- st_bbox(c(xmin = 11.98833, ymin = 56.56500, xmax = 12.72000, ymax = 57.40667), crs = st_crs(4326))
  }
  
  # Add dummy entries for full legend rendering of anomalies
  dummy_anomalies <- tibble::tibble(
    lon = NA, lat = NA,
    anomaly_swe = factor(setdiff(all_anomalies, unique(df$anomaly_swe)), levels = all_anomalies),
    extreme = factor("Inom historiskt intervall", levels = c("Inom historiskt intervall", "Över/under max/min")),
    combined_label = NA
  )
  
  # Add dummy entries for full legend rendering of extremes
  dummy_extremes <- tibble::tibble(
    lon = NA, lat = NA,
    anomaly_swe = factor("Normala värden", levels = all_anomalies),
    extreme = factor(c("Inom historiskt intervall", "Över/under max/min"),
                     levels = c("Inom historiskt intervall", "Över/under max/min")),
    combined_label = NA
  )
  
  # Combine real data with dummy points to ensure complete legends
  plot_df <- bind_rows(df, if(nrow(dummy_anomalies) > 0) dummy_anomalies, if(nrow(dummy_extremes) > 0) dummy_extremes)
  
  # Create the ggplot
  ggplot() +
    geom_sf(data = sw_coast, fill = "#eeeac4", color = "darkgrey", linewidth = .1) +
    geom_sf(data = lakes, fill = "lightblue", color = "darkgrey", linewidth = .1) +
    geom_path(data = rivers, aes(x = lon, y = lat), color = "lightblue", linewidth = 0.2, na.rm = TRUE) +
    geom_path(data = border, aes(x = lon, y = lat), color = "black", linewidth = 0.1, linetype = "dashed", na.rm = TRUE) +
    geom_point(data = plot_df, aes(x = lon, y = lat, fill = anomaly_swe, color = extreme),
               shape = 21, size = 4, stroke = 0.7, na.rm = TRUE) +
    scale_fill_manual(values = anomaly_colors_swe) +
    scale_color_manual(values = c("Inom historiskt intervall" = "black", "Över/under max/min" = "red")) +
    ggrepel::geom_text_repel(data = filter(plot_df, !is.na(combined_label)),
                             aes(x = lon, y = lat, label = combined_label),
                             size = 2, fontface = "bold") +
    coord_sf(xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad),
             ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad), expand = FALSE) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ) +
    labs(
      fill = "Avvikelse från referensvärde",
      color = "Extremvärde",
      title = paste0(parameter_map$parameter_name_plot[parameter_map$parameter_name == input$parameter], 
                     parameter_map$parameter_depth[parameter_map$parameter_name == input$parameter], ", ",
                     paste0(month_names_sv[as.numeric(input$month)], " ", input$year)),
      subtitle = "Jämförelse med referensperioden 2007–2016",
      x = "Longitud", y = "Latitud"
    ) +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2)
    )
}
