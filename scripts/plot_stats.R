library(dplyr)
library(ggplot2)
library(purrr)

# Load data
source(file.path("R", "load_data.R"))

# Function to plot timeseries for one dataframe
plot_timeseries <- function(df, dataset_name, outdir = "plots") {
  
  # Ensure subfolder exists
  dir_path <- file.path(outdir, dataset_name)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  # Apply filtering rule
  df_filtered <- df %>%
    group_by(station, parameter_name_short, month) %>%
    filter(
      if (first(parameter_name_short) == "Syre") {
        # Keep only rows where 'mean' is not NA, then pick deepest
        depth == max(depth[is.finite(mean)], na.rm = TRUE)
      } else {
        depth == 0
      }
    ) %>%
    ungroup()
  
  # Loop over parameters
  df_filtered %>%
    group_split(parameter_name_short) %>%
    walk(function(param_df) {
      
      param_name <- unique(param_df$parameter_name_short)
      
      p <- ggplot(param_df, aes(x = month)) +
        # Mean line
        geom_line(aes(y = mean, group = station, colour = "Mean"), size = 1) +
        # Ribbons for std and 2std
        geom_ribbon(aes(ymin = mean - std, ymax = mean + std, fill = "±1 SD"), alpha = 0.2) +
        geom_ribbon(aes(ymin = mean - `2std`, ymax = mean + `2std`, fill = "±2 SD"), alpha = 0.1) +
        # Min and max points as triangles
        geom_point(aes(y = min, shape = "Min"), size = 3) +
        geom_point(aes(y = max, shape = "Max"), size = 3) +
        scale_shape_manual(values = c("Min" = 25, "Max" = 24)) +  # triangle down/up
        # Facet by station
        facet_wrap(~station, scales = "free_y") +
        # Labels
        labs(
          title = paste0(param_name, " (", dataset_name, ")"),
          x = "Month",
          y = paste0("Value (", unique(param_df$parameter_unit), ")"),
          colour = NULL, fill = NULL, shape = NULL
        ) +
        # Legend to right
        theme_minimal() +
        theme(
          legend.position = "right",
          legend.title = element_blank()
        )
      
      ggsave(
        filename = file.path(dir_path, paste0(param_name, ".png")),
        plot = p,
        width = 15,
        height = 10,
        bg = "white"
      )
    })
}

# Loop over stats_list
walk2(stats_list, names(stats_list), ~ plot_timeseries(.x, .y, outdir = file.path("output", "plots", "reference_data")))
