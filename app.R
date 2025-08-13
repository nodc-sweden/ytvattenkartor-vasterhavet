#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(sf)
library(R.matlab)
library(ggrepel)
library(png)
library(jpeg)
library(tiff)
library(grid)
library(gridExtra)
library(ggpubr)

# Load helper functions and data
source("R/helper.R")
source("R/load_data.R")

# Define UI for application
ui <- fluidPage(
  titlePanel("Ytvattenkartor för Infocentralen Västerhavet"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Ladda upp InfoC-export (.txt)", accept = ".txt"),
      uiOutput("reference_data_ui"),
      uiOutput("year_ui"),
      selectInput("month", "Välj månad", choices = setNames(1:12, str_to_sentence(month_names_sv)), selected = 1),
      selectInput(
        "parameter",
        "Välj parameter",
        choices = setNames(
          parameter_map$parameter_name[-5][order(parameter_map$parameter_name_short[-5])],
          sort(parameter_map$parameter_name_short[-5])
        ),
        selected = "Chla"
      ),
      selectInput("bbox_option", "Välj kartutbredning", choices = c(
        "Bohuslän", "Halland", "Bohuslän och Halland", "Dynamisk"
      ), selected = "Bohuslän och Halland"),
      numericInput("plot_width", "Plotbredd, nedladdning (cm)", value = 15, min = 10, max = 100, step = 1),
      numericInput("plot_height", "Plothöjd, nedladdning (cm)", value = 20, min = 10, max = 100, step = 1),
      downloadButton("download_current_png", "Ladda ner aktuell plot (PNG)"),
      br(), br(),
      downloadButton("download_all_plots_zip", "Ladda ner plottar för alla parametrar (ZIP)"),
      br(), br(),
      div(
        tags$div(
          style = "font-weight: bold; margin-bottom: 4px; padding-left: 2px;",
          "Inkludera logos i månadsrapport"
        ),
        div(
          style = "display: flex; gap: 10px; align-items: center; margin-left: 3px;",
          tags$div(style = "margin: 0;", checkboxInput("include_logo_smhi", "SMHI", value = TRUE)),
          tags$div(style = "margin: 0;", checkboxInput("include_logo_bvvf", "BVVF", value = TRUE)),
          tags$div(style = "margin: 0;", checkboxInput("include_logo_lans", "NLST", value = TRUE))
        )
      ),
      downloadButton("download_all_plots_pdf", "Ladda ner månadsrapport (PDF)"),
    width = 3),
    mainPanel(
      plotOutput("map_plot", height = "800px")
    )
  ),
  
  # Footer
  tags$hr(),
  tags$footer(
    style = "text-align:center; padding:10px; font-size:0.9em; color:#666;",
    HTML(
      paste0(
        "Version ", pkg_version, " – ",
        "<a href='", github_url, "' target='_blank'>GitHub repository</a>"
      )
    ))
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
  
  # Example: if stats_list is preloaded in global environment
  output$reference_data_ui <- renderUI({
    selectInput(
      "reference_data",
      "Välj referensperiod",
      choices = names(stats_list),
      selected = names(stats_list)[1]
    )
  })
  
  # Access the chosen dataframe like this:
  selected_stats <- reactive({
    stats_list[[input$reference_data]]
  })
  
  # Define a reactive expression to read and preprocess the uploaded data file
  uploaded_data <- reactive({
    # Ensure a file has been uploaded before proceeding (halts reactivity if not)
    req(input$data_file)
    
    data_upload <- read_tsv(input$data_file$datapath, locale = locale(encoding = "ISO-8859-1"), col_types = cols(), 
                            progress = FALSE, name_repair = "unique_quiet")
    
    # Check for garbled characters in Station names
    bad_stations <- unique(data_upload$Station[has_garbled_utf8(data_upload$Station)])
    
    if (length(bad_stations) > 0) {
      showNotification(
        paste0(
          "Varning: Följande stationsnamn verkar innehålla felaktig teckenkodning (å/ä/ö):\n",
          paste(bad_stations, collapse = ", "),
          "\n\nKontrollera att filen är sparad i ISO-8859-1 eller använd rätt encoding."
        ),
        type = "error",
        duration = 15
      )
    }
    
    # Rename specific stations for consistency with plotting/statistical datasets
    data_upload$Station[data_upload$Station == "KOSTERFJORDEN"] <- "KOSTERFJORDEN (NR16)"
    data_upload$Station[data_upload$Station == "N7 OST NIDINGEN SLA 0-10"] <- "N7 OST NIDINGEN"
    
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
    
    # Convert lat/lon to decimal degrees
    data_upload$lat <- convert_dmm_to_dd(as.numeric(data_upload$Lat))
    data_upload$lon <- convert_dmm_to_dd(as.numeric(data_upload$Lon))
    
    data_upload
  })
  
  # Reactive that returns station depths for the selected
  # parameter/date — bottom depth for O2_CTD, else surface (0 m).
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
  
  # Reactive expression that returns the uploaded dataset
  # filtered by the selected year, month, and parameter,
  # and enriched with statistical reference values and anomaly
  # classifications.
  data_joined <- reactive({
    req(uploaded_data(), input$year, input$month, input$parameter)
    prepare_joined_data(uploaded_data(), input$parameter, input$year, input$month,
                        selected_stats(), all_anomalies)
  })
  
  # Define the reactive output for rendering the map plot
  output$map_plot <- renderPlot({
    df <- tryCatch({
      data_joined()
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(df) || nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Ingen data tillgänglig för vald parameter, månad och år.", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      create_plot(df, input, all_anomalies, anomaly_colors_swe, month_names_sv, parameter_map)
    }
  }, res = 120)
  
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
  
  # Define the download handler for downloading all PNGs for the selection as a ZIP file
  output$download_all_plots_zip <- downloadHandler(
    filename = function() {
      paste0("all_parameters_", input$year, "_", input$month, ".zip")
    },
    # Function that creates the content of the ZIP file
    content = function(zipfile) {
      temp_dir <- tempdir()
      files <- c()
      
      # Loop over each parameter in the parameter mapping table
      for (param in parameter_map$parameter_name) {
        # Skip this iteration if parameter is H2S (not plotted)
        if (param == "H2S") next
        
        # Construct file path for the PNG output
        param_short <- parameter_map$parameter_name_short[parameter_map$parameter_name == param]
        file_path <- file.path(
          temp_dir,
          paste0(make.names(gsub("ä", "", param_short)), "_", input$year, "_", input$month, ".png")
        )
        
        # Generate and save the parameter plot using a helper function
        file_saved <- save_param_plot(
          param, input$year, input$month, uploaded_data(), selected_stats(), all_anomalies,
          anomaly_colors_swe, month_names_sv, parameter_map, input$bbox_option,
          input$plot_width, input$plot_height, file_path
        )
        
        # If a plot was successfully saved, add it to the files list
        if (!is.null(file_saved)) files <- c(files, file_saved)
      }
      
      # Create a ZIP archive from all generated plot files
      utils::zip(zipfile, files = files, flags = "-j")
    }
  )
  
  # Define the download handler for downloading all plots for the selection as a PDF report (including a logo page)
  output$download_all_plots_pdf <- downloadHandler(
    filename = function() {
      paste0("plots_", input$year, "_", input$month, ".pdf")
    },
    content = function(pdf_file) {
      df_orig <- uploaded_data()
      temp_dir <- tempdir()
      plots <- list()
      
      # Title/logo page (uses checkboxes)
      logo_page <- create_logo_page(
        include_smhi = input$include_logo_smhi,
        include_bvvf = input$include_logo_bvvf,
        include_lans  = input$include_logo_lans,
        month = input$month,
        year = input$year,
        month_names_sv = month_names_sv
      )
      plots[[length(plots) + 1]] <- logo_page
      
      # Loop parameters and reuse prepare_joined_data / create_plot_for_param
      for (param in parameter_map$parameter_name) {
        if (param == "H2S") next
        
        # Quick check: is there any raw data for this parameter in the chosen month/year?
        df_filtered <- df_orig %>%
          filter(Year == input$year, `Month (calc)` == input$month)
        
        if (nrow(df_filtered) == 0 || all(is.na(df_filtered[[param]]))) next
        
        p <- create_plot_for_param(
          param = param,
          year = input$year,
          month = input$month,
          data = df_orig,
          stats_tidy = selected_stats(),
          all_anomalies = all_anomalies,
          anomaly_colors_swe = anomaly_colors_swe,
          month_names_sv = month_names_sv,
          parameter_map = parameter_map,
          bbox_option = input$bbox_option,
          plot_width = input$plot_width,
          plot_height = input$plot_height
        )
        
        if (!is.null(p)) plots[[length(plots) + 1]] <- p
      }
      
      if (length(plots) == 0) {
        stop("Inga plottar tillgängliga för PDF-export.")
      }
      
      # Write plots to PDF: first page full-size title/logo, subsequent plots centered in a viewport
      pdf(pdf_file, width = 8.27, height = 11.69, onefile = TRUE)
      
      # Print logo/title page (first element)
      print(plots[[1]])
      
      if (length(plots) > 1) {
        plot_width_in  <- input$plot_width / 2.54
        plot_height_in <- input$plot_height / 2.54
        
        for (i in seq(2, length(plots))) {
          p <- plots[[i]]
          grid.newpage()
          vp <- viewport(
            width  = unit(plot_width_in,  "in"),
            height = unit(plot_height_in, "in"),
            x = 0.5, y = 0.5,
            just = c("center", "center")
          )
          pushViewport(vp)
          print(p, newpage = FALSE)
          popViewport()
        }
      }
      
      dev.off()
    }
  )
}

shinyApp(ui, server)