#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

require(shiny)
require(tidyverse)
require(sf)
require(R.matlab)
require(ggrepel)

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
    "Temperatur", "Salthalt", "Syrekoncentration", "Syremättnad", "H2S", "Fosfatkoncentration", 
    "DIN-koncentration", "Kiselkoncentration", "Klorofyllkoncentration", "Secchidjup"
  ),
  parameter_depth = c(rep("ytvattnet", 2), "bottenvattnet", rep("ytvattnet", 7))
)

# Define all Swedish anomaly labels (used for categorizing measurement deviation)
all_anomalies <- c(
  "Mycket högre än normalt", "Högre än normalt", "Normala värden",
  "Lägre än normalt", "Mycket lägre än normalt", "Ingen provtagning", "Saknar historiska värden"
)

# Define color codes for each anomaly level (used in maps and plots)
anomaly_colors_swe <- c(
  "Mycket högre än normalt" = "#d73027",
  "Högre än normalt" = "#fdae61",
  "Normala värden" = "#66bd63",
  "Lägre än normalt" = "#91bfdb",
  "Mycket lägre än normalt" = "#313695",
  "Ingen provtagning" = "grey70",
  "Saknar historiska värden" = "white"
)

# Swedish month names (used for dropdowns, labels, etc.)
month_names_sv <- c("januari", "februari", "mars", "april", "maj", "juni",
                    "juli", "augusti", "september", "oktober", "november", "december")


# Load helper functions
source("R/helper.R")

# Define UI for application
ui <- fluidPage(
  titlePanel("Ytvattenkartor för Infocentralen Västerhavet"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Ladda upp InfoC-export (.txt)", accept = ".txt"),
      uiOutput("year_ui"),
      selectInput("month", "Välj månad", choices = setNames(1:12, str_to_sentence(month_names_sv)), selected = 1),
      selectInput(
        "parameter",
        "Välj parameter",
        choices = setNames(
          parameter_map$parameter_name[-5],
          parameter_map$parameter_name_short[-5]
        ),
        selected = "Temp CTD (prio CTD)"
      ),
      # numericInput("depth", "Välj djup (m)", value = 0, step = 1),
      hr(),
      selectInput("bbox_option", "Välj kartutbredning", choices = c(
        "Bohuslän", "Halland", "Bohuslän och Halland", "Dynamisk"
      ), selected = "Bohuslän och Halland"),
      numericInput("plot_width", "Plotbredd, nedladdning (cm)", value = 15, min = 5, max = 100, step = 1),
      numericInput("plot_height", "Plothöjd, nedladdning (cm)", value = 20, min = 5, max = 100, step = 1),
      downloadButton("download_current_png", "Ladda ner aktuell plot (PNG)"),
      br(), br(),
      downloadButton("download_all_plots_zip", "Ladda ner plottar för alla parametrar (ZIP)")
    ),
    mainPanel(
      plotOutput("map_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically render a selectInput for available years based on uploaded data
  output$year_ui <- renderUI({
    # If no file has been uploaded yet, return an empty selectInput as a placeholder
    if (is.null(input$data_file)) {
      selectInput(
        "year", "Välj år",
        choices = character(0),
        selected = character(0)
      )
    } else {
      # If a file has been uploaded, extract the available years from the dataset
      data <- uploaded_data()
      available_years <- sort(unique(data$Year))
      selectInput(
        "year", "Välj år",
        choices = available_years,
        selected = max(available_years, na.rm = TRUE)
      )
    }
  })
  
  # Reactive expression to read and tidy station statistics from a .mat file
  stats <- reactive({
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
    
    # Return the tidy data frame
    stats_tidy
  })
  
  # Define a reactive expression to read and preprocess the uploaded data file
  uploaded_data <- reactive({
    # Ensure a file has been uploaded before proceeding (halts reactivity if not)
    req(input$data_file)
    
    data_upload <- read_tsv(input$data_file$datapath, locale = locale(encoding = "ISO-8859-1"), col_types = cols(), 
                            progress = FALSE, name_repair = "unique_quiet")
    
    # Rename specific station for consistency with plotting/statistical datasets
    data_upload$Station[data_upload$Station == "KOSTERFJORDEN"] <- "KOSTERFJORDEN (NR16)"
    
    # Ensure the 'Month (calc)' column is numeric (in case it's been read as character)
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
    
    data_upload
  })
  
  selected_depths <- reactive({
    data <- uploaded_data()
    req(data, input$parameter, input$year, input$month)
    
    if (input$parameter == "O2_CTD (prio CTD)") {
      # Bottom depths
      data %>%
        filter(Year == input$year, `Month (calc)` == input$month) %>%
        filter(!is.na(.data[[input$parameter]])) %>%
        group_by(Station) %>%
        slice_max(Depth, with_ties = FALSE) %>%
        ungroup() %>%
        transmute(Station = toupper(Station), depth = as.integer(Depth))
    } else {
      # Surface depth
      data %>%
        filter(Year == input$year, `Month (calc)` == input$month) %>%
        filter(!is.na(.data[[input$parameter]])) %>%
        distinct(Station) %>%
        transmute(Station = toupper(Station), depth = 0L)
    }
  })
  
  # Define a reactive expression that joins uploaded data with statistics for plotting/analyzing
  data_joined <- reactive({
    data <- uploaded_data()
    req(data, input$year, input$month)
    
    data <- data %>%
      filter(Year == input$year, `Month (calc)` == input$month)
    
    data$lat <- convert_dmm_to_dd(as.numeric(data$Lat))
    data$lon <- convert_dmm_to_dd(as.numeric(data$Lon))
    
    # Use selected depths
    depth_df <- selected_depths()
    
    # Join depths to data
    data <- data %>%
      mutate(Station = toupper(Station)) %>%
      left_join(depth_df, by = "Station") %>%
      filter(Depth == depth)
    
    # Get stats for the correct parameter
    stat_param <- stats() %>%
      filter(parameter_name == input$parameter)
    
    # For each station, find the closest depth *below or equal* to sampled depth
    stat_best_match <- depth_df %>%
      rowwise() %>%
      mutate(depth_stat = {
        possible_depths <- stat_param %>%
          filter(station == Station) %>%
          pull(depth)
        
        if (length(possible_depths) == 0) {
          NA_integer_
        } else if (input$parameter == "O2_CTD (prio CTD)") {
          max(possible_depths, na.rm = TRUE)
        } else {
          0L
        }
      }) %>%
      ungroup()
    
    # Now join using the adjusted depth
    stat <- stat_param %>%
      inner_join(stat_best_match, by = c("station" = "Station", "depth" = "depth_stat"))
    
    joined <- data %>%
      filter(!is.na(.data[[input$parameter]])) %>%
      mutate(
        Month = as.integer(`Month (calc)`),
        depth = as.integer(Depth)
      ) %>%
      left_join(stat, by = c("Station" = "station", "Month" = "month", "depth" = "depth.y")) %>%
      mutate(
        pie_fill = mapply(assign_pie_fill, .data[[input$parameter]], mean, std, `2std`),
        value = .data[[input$parameter]],
        anomaly_swe = case_when(
          value < mean - `2std` ~ "Mycket lägre än normalt",
          value >= mean - `2std` & value < mean - std ~ "Lägre än normalt",
          value >= mean - std & value <= mean + std ~ "Normala värden",
          value > mean + std & value <= mean + `2std` ~ "Högre än normalt",
          value > mean + `2std` ~ "Mycket högre än normalt",
          is.na(value) & !is.na(mean) ~ "Ingen provtagning",
          is.na(mean) ~ "Saknar historiska värden"
        ),
        anomaly_swe = factor(anomaly_swe, levels = all_anomalies),
        extreme = factor(case_when(
          value < min ~ "Över/under max/min",
          value > max ~ "Över/under max/min",
          TRUE        ~ "Inom historiskt intervall"
        ), levels = c("Inom historiskt intervall", "Över/under max/min")),
        combined_label = paste0(Station, "\n", round(value, 2))
      )
    
    joined
  })
  
  # Define the reactive output for rendering the map plot
  output$map_plot <- renderPlot({
    df <- data_joined()
    req(nrow(df) > 0)
    create_plot(df, input, all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map)
  })
  
  # Define the download handler for downloading the current map plot as a PNG image
  output$download_current_png <- downloadHandler(
    filename = function() {
      param_short <- parameter_map$parameter_name_short[parameter_map$parameter_name == input$parameter]
      param_short <- gsub("ä", "", param_short)
      paste0(param_short, "_", input$year, "_", input$month, ".png")
    },
    content = function(file) {
      df <- data_joined()
      ggsave(file, plot = create_plot(df, input, all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map), width = input$plot_width / 2.54, height = input$plot_height / 2.54, dpi = 300, bg = "white")
    }
  )
  
  # Download handler that lets the user download all plots for all parameters as a ZIP file
  output$download_all_plots_zip <- downloadHandler(
    filename = function() {
      paste0("all_parameters_", input$year, "_", input$month, ".zip")
    },
    
    # Function that generates the content of the ZIP file
    content = function(zipfile) {
      df_orig <- uploaded_data()
      temp_dir <- tempdir()
      files <- c()
      
      req(uploaded_data(), input$year, input$month)
      
      for (param in parameter_map$parameter_name) {
        if (param == "H2S") {
          next
        }
        
        param_short <- parameter_map$parameter_name_short[parameter_map$parameter_name == param]
        
        # Dynamically get depths per station based on current parameter
        depth_df <- {
          if (param == "O2_CTD (prio CTD)") {
            df_orig %>%
              filter(Year == input$year, `Month (calc)` == input$month) %>%
              filter(!is.na(.data[[param]])) %>%
              group_by(Station) %>%
              slice_max(Depth, with_ties = FALSE) %>%
              ungroup() %>%
              transmute(Station = toupper(Station), depth = as.integer(Depth))
          } else {
            df_orig %>%
              filter(Year == input$year, `Month (calc)` == input$month) %>%
              filter(!is.na(.data[[param]])) %>%
              distinct(Station) %>%
              transmute(Station = toupper(Station), depth = 0L)
          }
        }
        
        depth_df <- depth_df %>%
          mutate(Station = toupper(Station))
        
        # Prepare data for plotting
        df <- df_orig %>%
          filter(Year == input$year, `Month (calc)` == input$month) %>%
          mutate(Station = toupper(Station)) %>%
          left_join(depth_df, by = "Station") %>%
          filter(Depth == depth)
        
        df$lat <- convert_dmm_to_dd(as.numeric(df$Lat))
        df$lon <- convert_dmm_to_dd(as.numeric(df$Lon))
        
        # Get stats for the correct parameter
        stat_param <- stats() %>%
          filter(parameter_name == param) %>%
          mutate(station = toupper(station))
        
        # For each station, find the closest depth *below or equal* to sampled depth
        stat_best_match <- depth_df %>%
          rowwise() %>%
          mutate(depth_stat = {
            station_depths <- stat_param %>%
              filter(station == Station) %>%
              pull(depth)
            
            if (length(station_depths) == 0 || all(is.na(station_depths))) {
              NA_integer_
            } else if (param == "O2_CTD (prio CTD)") {
              max(station_depths, na.rm = TRUE)
            } else {
              0L
            }
          }) %>%
          ungroup()
        
        stat_best_match <- stat_best_match %>%
          filter(!is.na(depth_stat))
        
        # Now join using the adjusted depth
        stat <- stat_param %>%
          inner_join(stat_best_match, by = c("station" = "Station", "depth" = "depth_stat"))
        
        joined <- df %>%
          filter(!is.na(.data[[param]])) %>%
          mutate(
            Month = as.integer(`Month (calc)`),
            depth = as.integer(Depth)
          ) %>%
          left_join(stat, by = c("Station" = "station", "Month" = "month", "depth" = "depth.y")) %>%
          mutate(
            pie_fill = mapply(assign_pie_fill, .data[[param]], mean, std, `2std`),
            value = .data[[param]],
            anomaly_swe = case_when(
              value < mean - `2std` ~ "Mycket lägre än normalt",
              value >= mean - `2std` & value < mean - std ~ "Lägre än normalt",
              value >= mean - std & value <= mean + std ~ "Normala värden",
              value > mean + std & value <= mean + `2std` ~ "Högre än normalt",
              value > mean + `2std` ~ "Mycket högre än normalt",
              is.na(mean) ~ "Saknar historiska värden",
              TRUE ~ "Ingen provtagning"
            ),
            anomaly_swe = factor(anomaly_swe, levels = all_anomalies),
            extreme = factor(case_when(
              value < min ~ "Över/under max/min",
              value > max ~ "Över/under max/min",
              TRUE        ~ "Inom historiskt intervall"
            ), levels = c("Inom historiskt intervall", "Över/under max/min")),
            combined_label = paste0(Station, "\n", round(value, 2))
          )
        
        if (nrow(joined) > 0) {
          file_path <- file.path(temp_dir, paste0(make.names(gsub("ä", "", param_short)), "_", input$year, "_", input$month, ".png"))
          
          ggsave(file_path, plot = create_plot(joined, list(
            parameter = param,
            year = input$year,
            month = input$month,
            depth = NA,  # not used in this case
            bbox_option = input$bbox_option
          ), all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map),
          width = input$plot_width / 2.54, height = input$plot_height / 2.54, dpi = 300, bg = "white")
          
          files <- c(files, file_path)
        }
      }
      
      utils::zip(zipfile, files = files, flags = "-j")  # -j = junk paths
    }
  )
}

shinyApp(ui, server)