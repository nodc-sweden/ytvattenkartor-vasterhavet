# Conversion function
convert_dmm_to_dd <- function(dmm) {
  deg <- floor(dmm / 100)            # Extract degrees
  min <- dmm - deg * 100             # Extract minutes
  dd <- deg + min / 60               # Convert to decimal degrees
  return(dd)
}

has_garbled_utf8 <- function(x) {
  # Detect common mojibake patterns caused by UTF-8 misinterpreted as Latin1
  grepl("Ã.|Ã¥|Ã¤|Ã¶|Ã–|Ã„|Ã…|â|€|™", x)
}

# Function to read image and set custom height
read_image_as_grob <- function(path, height_inches = 1) {
  ext <- tools::file_ext(path)
  img <- switch(
    tolower(ext),
    "png"  = png::readPNG(path),
    "jpg"  = jpeg::readJPEG(path),
    "jpeg" = jpeg::readJPEG(path),
    "tif"  = tiff::readTIFF(path),
    "tiff" = tiff::readTIFF(path),
    stop(paste("Unsupported format:", ext))
  )
  rasterGrob(img, interpolate = TRUE, height = unit(height_inches, "inches"))
}

# Helper: join uploaded data with stats and add anomalies
prepare_joined_data <- function(data, param, year, selected_month, stats_tidy, all_anomalies) {
  # Filter year/month
  df_filtered <- data %>%
    filter(Year == year, `Month (calc)` == selected_month)
  
  # Depths from selected_depths logic
  depth_df <- if (param == "O2_CTD (prio CTD)") {
    df_filtered %>%
      filter(!is.na(.data[[param]])) %>%
      group_by(Station) %>%
      slice_max(Depth, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(Station = toupper(Station), depth = as.integer(Depth))
  } else {
    df_filtered %>%
      filter(!is.na(.data[[param]])) %>%
      distinct(Station) %>%
      transmute(Station = toupper(Station), depth = 0L)
  }
  
  if (nrow(depth_df) == 0) return(NULL)
  
  # Join depths to data
  df <- df_filtered %>%
    mutate(Station = toupper(Station)) %>%
    left_join(depth_df, by = "Station") %>%
    filter(Depth == depth)
  
  if (nrow(df) == 0) return(NULL)
  
  # Get stats for the correct parameter
  stat_param <- stats_tidy %>%
    filter(parameter_name == param) %>%
    mutate(station = toupper(station)) %>%
    filter(month == as.integer(selected_month)) %>%
    filter(!is.na(mean)) 
  
  if (nrow(stat_param) == 0) return(NULL)
  
  # Match stats depth
  stat_best_match <- depth_df %>%
    rowwise() %>%
    mutate(depth_stat = {
      station_depths <- stat_param %>%
        filter(station == Station) %>%
        filter(!is.na(mean)) %>%
        filter(month == selected_month) %>%
        pull(depth)
      
      if (length(station_depths) == 0 || all(is.na(station_depths))) {
        NA_integer_
      } else if (param == "O2_CTD (prio CTD)") {
        max(station_depths, na.rm = TRUE)
      } else {
        0L
      }
    }) %>%
    ungroup() %>%
    filter(!is.na(depth_stat))
  
  if (nrow(stat_best_match) == 0) return(NULL)
  
  # Join with stats and compute anomalies
  joined <- df %>%
    filter(!is.na(.data[[param]])) %>%
    mutate(
      Month = as.integer(`Month (calc)`),
      depth = as.integer(Depth)
    ) %>%
    left_join(stat_param %>%
                inner_join(stat_best_match, by = c("station" = "Station", "depth" = "depth_stat")),
              by = c("Station" = "station", "Month" = "month", "depth" = "depth.y")) %>%
    mutate(
      value = .data[[param]],
      anomaly_swe = case_when(
        value < mean - `2std` ~ "Mycket lägre än normalt",
        value >= mean - `2std` & value < mean - std ~ "Lägre än normalt",
        value >= mean - std & value <= mean + std ~ "Normala värden",
        value > mean + std & value <= mean + `2std` ~ "Högre än normalt",
        value > mean + `2std` ~ "Mycket högre än normalt",
        is.na(mean) ~ "Saknar referensvärde"
      ),
      anomaly_swe = factor(anomaly_swe, levels = all_anomalies),
      extreme = factor(case_when(
        value < min ~ "Under minimum",
        value > max ~ "Över maximum",
        TRUE ~ "Inom normalspann"
      ), levels = c("Över maximum", "Inom normalspann", "Under minimum")),
      combined_label = paste0(Station, "\n", round(value, 2))
    )
  
  if (nrow(joined) == 0) return(NULL)
  
  joined
}

# Helper: save plot for one parameter
save_param_plot <- function(param, year, month, data, stats_tidy, all_anomalies,
                            anomaly_colors_swe, month_names_sv, parameter_map,
                            bbox_option, plot_width, plot_height, out_path, add_shapes,
                            reference_data) {
  joined <- prepare_joined_data(data, param, year, month, stats_tidy, all_anomalies)
  if (is.null(joined)) return(NULL)
  
  ggsave(
    out_path,
    plot = create_plot(joined, list(
      parameter = param,
      year = year,
      month = month,
      depth = NA,
      bbox_option = bbox_option,
      add_shapes = add_shapes,
      reference_data = reference_data
    ), all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map),
    width = plot_width / 2.54,
    height = plot_height / 2.54,
    dpi = 300,
    bg = "white"
  )
  
  out_path
}

# Return a ggplot for a given parameter (or NULL if no data)
create_plot_for_param <- function(param, year, month, data, stats_tidy,
                                  all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map,
                                  bbox_option, plot_width, plot_height, add_shapes, reference_data) {
  joined <- prepare_joined_data(data, param, year, month, stats_tidy, all_anomalies)
  if (is.null(joined)) return(NULL)
  
  p <- create_plot(joined, list(
    parameter = param,
    year = year,
    month = month,
    depth = NA,
    bbox_option = bbox_option,
    add_shapes = add_shapes,
    reference_data = reference_data
  ), all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map)
  
  # apply a small margin so page layout looks consistent in PDF
  p + theme(plot.margin = unit(rep(0.2, 4), "cm"))
}

# Build a title/logo page as a ggplot (returns ggplot)
create_logo_page <- function(include_smhi, include_bvvf, include_lans, month, year, month_names_sv) {
  all_logos <- list(
    smhi = list(path = "assets/SMHI logotype svart RGB 52 mm.jpg", height = 1, include = include_smhi),
    bvvf = list(path = "assets/logo BVVF.tif",                    height = 1, include = include_bvvf),
    lans = list(path = "assets/Lansstyrelsen.jpg",                height = 1.5, include = include_lans)
  )
  
  selected_logos <- Filter(function(x) isTRUE(x$include), all_logos)
  
  if (length(selected_logos) > 0) {
    logo_paths   <- vapply(selected_logos, `[[`, "", "path")
    logo_heights <- vapply(selected_logos, `[[`, 0,  "height")
    logos <- mapply(read_image_as_grob, logo_paths, logo_heights, SIMPLIFY = FALSE)
    
    spacer <- nullGrob()
    spacer_height <- 0.3
    stack_grobs <- list()
    stack_heights <- c()
    
    for (i in seq_along(logos)) {
      stack_grobs <- c(stack_grobs, list(logos[[i]]))
      stack_heights <- c(stack_heights, logo_heights[i])
      if (i < length(logos)) {
        stack_grobs <- c(stack_grobs, list(spacer))
        stack_heights <- c(stack_heights, spacer_height)
      }
    }
    
    stack_heights <- unit(stack_heights, "inches")
    logo_stack <- arrangeGrob(grobs = stack_grobs, ncol = 1, heights = stack_heights)
  } else {
    logo_stack <- nullGrob()
  }
  
  title_text <- paste0("Ytvattenkartor Västerhavet, ", month_names_sv[as.integer(month)], " ", year)
  
  ggplot() +
    theme_void() +
    ggtitle(title_text) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    annotation_custom(
      grob = logo_stack,
      xmin = -Inf, xmax = Inf,
      ymin = 0.5, ymax = 0.5
    )
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
    extreme = factor("Inom normalspann", levels = c("Över maximum", "Inom normalspann", "Under minimum")),
    combined_label = NA
  )
  
  # Add dummy entries for full legend rendering of extremes
  dummy_extremes <- tibble::tibble(
    lon = NA, lat = NA,
    anomaly_swe = factor("Normala värden", levels = all_anomalies),
    extreme = factor(c("Över maximum", "Inom normalspann", "Under minimum"),
                     levels = c("Över maximum", "Inom normalspann", "Under minimum")),
    combined_label = NA
  )
  
  # Combine real data with dummy points to ensure complete legends
  plot_df <- bind_rows(df, if(nrow(dummy_anomalies) > 0) dummy_anomalies, if(nrow(dummy_extremes) > 0) dummy_extremes)
  
  # Define aesthetics conditionally
  if (input$add_shapes) {
    point_aes <- aes(x = lon, y = lat, fill = anomaly_swe, shape = extreme)
    shape_fixed <- NULL
  } else {
    point_aes <- aes(x = lon, y = lat, fill = anomaly_swe)
    shape_fixed <- 21
  }
  
  # Create the ggplot
  p <- ggplot() +
    geom_sf(data = sw_coast, fill = "#eeeac4", color = "darkgrey", linewidth = .1) +
    geom_sf(data = lakes, fill = "lightblue", color = "darkgrey", linewidth = .1) +
    geom_path(data = rivers, aes(x = lon, y = lat), color = "lightblue", linewidth = 0.2, na.rm = TRUE) +
    geom_path(data = border, aes(x = lon, y = lat), color = "black", linewidth = 0.1, linetype = "dashed", na.rm = TRUE)
  
  # Add points with conditional aesthetics
  if (input$add_shapes) {
    p <- p + geom_point(data = plot_df, aes(x = lon, y = lat, fill = anomaly_swe, shape = extreme),
                        size = 4, stroke = 0.7, color = "black", na.rm = TRUE)
  } else {
    p <- p + geom_point(data = plot_df, aes(x = lon, y = lat, fill = anomaly_swe),
                        size = 4, shape = 21, stroke = 0.7, color = "black", na.rm = TRUE)
  }
  
  # Add labels and customize the plot
  p <- p + scale_fill_manual(values = anomaly_colors_swe) +
    ggrepel::geom_text_repel(
      data = filter(plot_df, !is.na(combined_label)),
      aes(x = lon, y = lat, label = combined_label),
      size = 2, fontface = "bold"
    ) +
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
      fill = "Avvikelse från medelvärde",
      title = paste0(
        parameter_map$parameter_name_plot[parameter_map$parameter_name == input$parameter], " (",
        parameter_map$parameter_unit[parameter_map$parameter_name == input$parameter], ")",
        parameter_map$parameter_depth[parameter_map$parameter_name == input$parameter], ", ",
        paste0(month_names_sv[as.numeric(input$month)], " ", input$year)
      ),
      subtitle = paste("Jämförelse med referensperioden", input$reference_data),
      x = "Longitud", y = "Latitud"
    ) +
    guides(
      fill = guide_legend(
        order = 1,
        override.aes = list(shape = 21, color = "black") # makes fill legend use filled shape
      )
    )
  
  # Conditionally add the shape scale + legend
  if (input$add_shapes) {
    p <- p +
      scale_shape_manual(
        name = "Avvikelse från referensintervall",
        values = c(
          "Över maximum"     = 24,
          "Inom normalspann" = 21,
          "Under minimum"    = 25
        )
      ) +
      guides(
        shape = guide_legend(
          order = 2,
          override.aes = list(fill = "white", color = "black")
        )
      )
  }
  
  # Return the final plot
  p
}