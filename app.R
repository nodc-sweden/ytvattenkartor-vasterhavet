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
library(ggrepel)
library(png)
library(jpeg)
library(tiff)
library(grid)
library(gridExtra)
library(ggpubr)
library(plotly)
library(SHARK4R)
library(DT)
library(tidyr)
library(markdown)
library(gsw)

# Load helper functions and data (these define functions like prepare_joined_data, create_plot, etc.)
source(file.path("R", "helper.R"))
source(file.path("R", "load_data.R"))

# Define UI for application
ui <- fluidPage(
  titlePanel("Ytvattenkartor för Infocentralen Västerhavet"),
  
  tabsetPanel(
    tabPanel("Kartdiagram",
             sidebarLayout(
               sidebarPanel(
                 fileInput("data_file", "Ladda upp InfoC-export (.txt)", accept = ".txt", placeholder = "t.ex. BH_19_21_2025-05-27.txt"),
                 uiOutput("reference_data_ui"),
                 div(
                   tags$div(
                     style = "font-weight: bold; margin-bottom: 4px; padding-left: 2px;",
                     "Visa avvikelse från referensintervall"
                   ),
                   div(
                     style = "display: flex; gap: 10px; align-items: center; margin-left: 3px;",
                     tags$div(style = "margin: 0;", checkboxInput("add_shapes", "Ja", value = FALSE)),
                     tags$div(style = "margin: 0;", checkboxInput("only_flanks", "Enbart vid 'Mycket högre/lägre än normalt'", value = FALSE))
                   )
                 ),
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
                 width = 3
               ),
               mainPanel(
                 plotOutput("map_plot", height = "800px")
               )
             )
    ),
    
    tabPanel("Utforska referensdata",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("ref_year_ui"),
                 # These inputs are dynamic because stats_list is reactive
                 uiOutput("ref_param_ui"),
                 uiOutput("ref_dataset_ui"),
                 uiOutput("ref_station_ui"),
                 width = 3
               ),
               mainPanel(
                 plotlyOutput("ref_plot", height = "800px")
               )
             )
    ),
    tabPanel("Uppdatera referensdata",
             sidebarLayout(
               sidebarPanel(
                 numericInput("to_year", "Uppdatera till och med år:", value = as.integer(format(Sys.Date(), "%Y")) - 1),
                 numericInput("time_range", "Tidsspann (år):", value = 10, min = 1),
                 numericInput("min_n", "Minsta antal mätningar för medelvärde:", value = 3, min = 1),
                 checkboxGroupInput("platform_filter", 
                                    label = HTML('Välj plattformskoder (<a href="https://smhi.se/oceanografi/oce_info_data/shark_web/downloads/codelist_SMHI.xlsx" target="_blank">se aktuell lista här</a>):'),
                                    choices = platform_codes,
                                    selected = platform_codes),
                 textInput("platform_custom", "Ange andra plattformskoder (separerade med komma-tecken):",
                           placeholder = "t.ex. 77WX, 77K9"),
                 textInput("station_custom", 
                           HTML('Lägg till ytterligare stationer än de <a href="https://github.com/nodc-sweden/ytvattenkartor-vasterhavet/blob/f4a22cb947405553fae5151ebcbb8d53c453423f/data/config/station_names.txt" target="_blank">fördefinierade</a>:'),
                           placeholder = "t.ex. BY10, BY15 GOTLANDSDJ"),
                 actionButton("update_ref", "Uppdatera referensdata"),
                 width = 3
               ),
               mainPanel(
                 DT::dataTableOutput("ref_table")
               )
             )
    ),
    tabPanel("README",
             fluidRow(
               column(
                 width = 10, offset = 1,
                 includeMarkdown("README.md")
               )
             )
    )
  ),
  
  tags$hr(),
  tags$footer(
    style = "text-align:center; padding:10px; font-size:0.9em; color:#666;",
    HTML(
      paste0(
        "Version ", pkg_version, " – ",
        "<a href='", github_url, "' target='_blank'>GitHub repository</a>"
      )
    )
  )
)

server <- function(input, output, session) {
  
  stats_list <- reactiveVal(
    readRDS(file.path("data", "reference_data", "reference_data.rds"))
  )
  
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
  
  # Dynamically render a selectInput for available years based on uploaded data
  output$ref_year_ui <- renderUI({
    upload_years <- NULL
    if (!is.null(input$data_file)) {
      upload_years <- sort(unique(uploaded_data()$Year))
    }
    
    # Only create selector if we have years
    if (length(upload_years) > 0) {
      selectInput(
        "ref_year", "Välj år",
        choices = upload_years,
        selected = max(upload_years, na.rm = TRUE)
      )
    } else {
      # Show a disabled selector so UI doesn't break
      selectInput("ref_year", "Välj år", choices = c("Ingen uppladdad data" = ""), selected = "")
    }
  })
  
  # Reference-data related UI: dataset / parameter / station
  output$reference_data_ui <- renderUI({
    selectInput(
      "reference_data",
      "Välj referensperiod",
      choices = names(stats_list()),
      selected = names(stats_list())[1]
    )
  })
  
  output$ref_dataset_ui <- renderUI({
    req(stats_list())
    selectInput(
      "ref_dataset",
      "Välj dataset",
      choices = names(stats_list()),
      selected = names(stats_list())[1]
    )
  })
  
  output$ref_param_ui <- renderUI({
    req(stats_list())
    choices <- sort(
      setdiff(
        unique(unlist(lapply(stats_list(), function(df) unique(df$parameter_name_short)))),
        "H2S"
      )
    )
    selectInput("ref_param", "Välj parameter", choices = choices, selected = choices[1])
  })
  
  output$ref_station_ui <- renderUI({
    # Build station choices from selected dataset and parameter
    req(stats_list(), input$ref_dataset)
    
    # Get the stats dataframe for the selected dataset
    stats <- stats_list()[[input$ref_dataset]]
    
    # If no stats or no stations, return empty choices
    if (is.null(stats) || nrow(stats) == 0 || all(is.na(stats$station))) {
      stations <- character(0)
    } else {
      stations <- sort(unique(stats$station))
    }
    
    selectInput(
      "ref_station", "Välj station",
      choices = stations,
      selected = if (length(stations) > 0) stations[1] else NULL
    )
  })
  
  # Access the chosen dataframe
  selected_stats <- reactive({ 
    req(input$reference_data) 
    stats_list()[[input$reference_data]] 
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
    data_upload$DIN <- calculate_DIN(data_upload$NO2, data_upload$NO3, data_upload$NH4)
    
    # Convert lat/lon to decimal degrees
    data_upload$lat <- convert_dmm_to_dd(as.numeric(data_upload$Lat))
    data_upload$lon <- convert_dmm_to_dd(as.numeric(data_upload$Lon))
    
    data_upload
  })
  
  # Reactive expression that returns the uploaded dataset
  # filtered by the selected year, month, and parameter,
  # and enriched with statistical reference values and anomaly
  # classifications.
  data_joined <- reactive({
    req(uploaded_data(), input$year, input$month, input$parameter)
    prepare_joined_data(uploaded_data(), input$parameter, input$year, input$month,
                        selected_stats(), all_anomalies, input$only_flanks)
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
      nid <- showNotification("ZIP-generering startad. Detta kan ta några sekunder...", duration = NULL)
      
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
          input$plot_width, input$plot_height, file_path, input$add_shapes, input$reference_data,
          input$only_flanks
        )
        
        # If a plot was successfully saved, add it to the files list
        if (!is.null(file_saved)) files <- c(files, file_saved)
      }
      
      # Create a ZIP archive from all generated plot files
      utils::zip(zipfile, files = files, flags = "-j")
      removeNotification(nid)
    }
  )
  
  # Define the download handler for downloading all plots for the selection as a PDF report (including a logo page)
  output$download_all_plots_pdf <- downloadHandler(
    filename = function() {
      paste0("Ytvattenkartor ", input$bbox_option, " ", month_names_sv[as.integer(input$month)], ".pdf")
    },
    content = function(pdf_file) {
      nid <- showNotification("PDF-generering startad. Detta kan ta några sekunder...", duration = NULL)
      
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
          plot_height = input$plot_height,
          add_shapes = input$add_shapes,
          reference_data = input$reference_data,
          only_flanks = input$only_flanks
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
      removeNotification(nid)
    }
  )
  
  # Render reference data plot
  output$ref_plot <- renderPlotly({
    req(input$ref_dataset, input$ref_param, input$ref_station, stats_list())
    
    # Filter the dataset for the selected parameter and station
    df <- stats_list()[[input$ref_dataset]] %>%
      filter(parameter_name_short == input$ref_param,
             station == input$ref_station)
    
    # Select the relevant depth:
    # - If the parameter is "Syre" (oxygen), pick the maximum depth with valid mean values
    # - Otherwise, use surface (depth == 0)
    df <- df %>%
      group_by(station, parameter_name_short, month) %>%
      filter(
        if (!is.na(first(parameter_name_short)) &&
            first(parameter_name_short) == "Syre") {
          depth == max(depth[is.finite(mean)], na.rm = TRUE)
        } else {
          depth == 0
        }
      ) %>%
      ungroup()
    
    # Handle case when no data is available for the selection
    if (is.null(df) || nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Ingen data tillgänglig för vald parameter, dataset och station.",
                 size = 6, hjust = 0.5) +
        theme_void()
      return(ggplotly(p))
    } else {
      
      # Create hover text for plotly tooltips
      df <- df %>%
        dplyr::mutate(
          hover_mean = paste0("Månad: ", str_to_sentence(month_names_sv)[month],
                              "<br>Medel: ", round(mean, 2),
                              "<br>Std: ", round(std, 2)),
          hover_min = paste0("Månad: ", str_to_sentence(month_names_sv)[month],
                             "<br>Min: ", round(min, 2)),
          hover_max = paste0("Månad: ", str_to_sentence(month_names_sv)[month],
                             "<br>Max: ", round(max, 2))
        )
      
      # Compute depth range for title annotation
      depth <- range(df$depth)
      depth <- paste(unique(depth), collapse = "-")
      
      suppressWarnings({
        # Build ggplot
        p <- ggplot(df, aes(x = month)) +
          geom_line(aes(y = mean, colour = "Mean", group = 1, text = hover_mean), size = 1) +
          geom_point(aes(y = mean, colour = "Mean", text = hover_mean), alpha = 0) +
          geom_ribbon(aes(ymin = mean - std, ymax = mean + std, fill = "±1 SD"), alpha = 0.2) +
          geom_ribbon(aes(ymin = mean - `2std`, ymax = mean + `2std`, fill = "±2 SD"), alpha = 0.1) +
          geom_point(aes(y = min, shape = "Min", text = hover_min), size = 3) +
          geom_point(aes(y = max, shape = "Max", text = hover_max), size = 3) +
          scale_shape_manual(values = c("Min" = 25, "Max" = 24)) +
          labs(
            title = paste0(input$ref_param, " (", input$ref_dataset, ")", ", ", depth, " m"),
            x = "Månad",
            y = paste0("Värde (", unique(df$parameter_unit), ")")
          ) +
          theme_minimal() +
          theme(legend.position = "right")
      })
      
      if (!is.null(input$data_file) && length(input$ref_year) > 0) {
        
        parameter <- parameter_map[parameter_map$parameter_name_short == input$ref_param,]$parameter_name
        
        # Select
        df_filtered <- uploaded_data() %>%
          filter(Year == input$ref_year, Station == input$ref_station) 
        
        # Depths from selected_depths logic
        depth_df <- get_depth_df_ref(df_filtered, parameter)
        
        # Join depths to data
        df <- df_filtered %>%
          mutate(Station = toupper(Station)) %>%
          rename(value = !!sym(parameter),
                 month = `Month (calc)`) %>%
          left_join(depth_df, by = c("Station", "month")) %>%
          filter(Depth == depth) %>%
          dplyr::mutate(
            hover_value = paste0("Månad: ", str_to_sentence(month_names_sv)[month],
                                "<br>Mätvärde: ", round(value, 2)
          ))
        
        suppressWarnings({
          p <- p +
            geom_point(
              data = df,
              aes(x = month, y = value, colour = "Current value", text = hover_value),
              size = 3,
              shape = 16,
              alpha = 0.7
            ) +
            scale_colour_manual(
              values = c("Mean" = "red", "Current value" = "green")
            )
        })
        
      }
      
      # Convert to interactive plotly plot with custom tooltips
      ggplotly(p, tooltip = "text")
    }
  })
  
  # Observe event for updating reference data from SHARK
  observeEvent(input$update_ref, {
    showNotification("Uppdaterar referensdata...", type = "message", id = "update_ref_msg", duration = NULL)
    
    # From checkboxes
    selected_platforms <- input$platform_filter
    
    # From free text (split on commas or whitespace, trim spaces)
    if (nzchar(input$platform_custom)) {
      extra_codes <- unlist(strsplit(input$platform_custom, "[, ]+"))
      extra_codes <- trimws(extra_codes)  # remove stray spaces
      extra_codes <- extra_codes[nzchar(extra_codes)]  # drop empty
      selected_platforms <- unique(c(selected_platforms, extra_codes))
    }
    
    # From pre-defined list
    selected_stations <- station_names
    
    # From free text (split on commas or whitespace, trim spaces)
    if (nzchar(input$station_custom)) {
      extra_stations <- unlist(strsplit(input$station_custom, "[, ]+"))
      extra_stations <- trimws(extra_stations)  # remove stray spaces
      extra_stations <- extra_stations[nzchar(extra_stations)]  # drop empty
      selected_stations <- unique(c(selected_stations, extra_stations))
    }
    
    tryCatch({
      update_stats(input$to_year, input$time_range, stats_list(), selected_stations, parameter_map, input$min_n, selected_platforms)
      
      # Reload updated reference data
      updated_stats <- readRDS("data/reference_data/reference_data.rds")
      stats_list(updated_stats)
      
      removeNotification(id = "update_ref_msg")
      showNotification("Referensdata uppdaterad!", type = "message", duration = 5)
    }, error = function(e) {
      removeNotification(id = "update_ref_msg")
      showNotification(paste("Fel vid uppdatering:", e$message), type = "error", duration = 10)
    })
  })
  
  output$ref_table <- DT::renderDataTable({
    req(stats_list())
    
    # Create a summary table for all datasets in stats_list
    summary_df <- lapply(names(stats_list()), function(name) {
      df <- stats_list()[[name]]
      
      # Filter out rows with NA in mean
      df <- df %>% dplyr::filter(!is.na(mean))
      
      tibble(
        Dataset = name,
        `Antal stationer` = n_distinct(df$station),
        `Antal parametrar` = n_distinct(df$parameter_id),
        `Antal medelvärden` = nrow(df),
        Källa = unique(df$source),
        `Senaste uppdatering` = unique(df$date_updated)
      )
    }) %>% 
      bind_rows()
    
    DT::datatable(
      summary_df,
      options = list(
        pageLength = 15,
        autoWidth = TRUE
      ),
      caption = "Sammanfattning av tillgängliga referensdataset",
    )
  })
}

shinyApp(ui, server)
