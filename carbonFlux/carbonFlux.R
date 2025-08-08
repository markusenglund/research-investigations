library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)

carbonFluxData <- read_excel("./Data_Dryad_Drought_Decreases_Carbon_Flux_but_Not_Transport_Speed_of_Newly_Fixed_Carbon_from_Leaves_to_Sinks_in_a_Giant_Bamboo_Forest.xlsx",
                             sheet = "Graph data")

create_plot <- function(data, y_variable, plot_title, y_label, unique_times, y_lim_low, y_lim_high) {
  ggplot(
    data = data,
    mapping = aes(x = time_seq, y = .data[[y_variable]], group = Treatment)
  ) +
    geom_point(aes(fill = Treatment), shape = 21, size = 3, color = "black", stroke = 1) +
    geom_line(aes(color = Treatment), size = 1) +
    scale_x_continuous(
      breaks = 1:length(unique_times),
      labels = unique_times
    ) +
    scale_color_manual(values = c("Control" = "blue", "Drought" = "red")) +
    scale_fill_manual(values = c("Control" = "blue", "Drought" = "red")) +
    labs(
      title = plot_title,
      x = "Sample Time (d)",
      y = y_label
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim = c(y_lim_low, y_lim_high))
}

ramet_types <- c("R0", "R1", "R2")
plot_list <- list()

y_limits_config <- list(
  "Leave 13C atom%" = list(
    "R0" = c(low = -0.2, high = 1.2),
    "R1" = c(low = -0.002, high = 0.014),
    "R2" = c(low = -0.02, high = 0.08)
  ),
  "Branches 13C atom%" = list(
    "R0" = c(low = -0.1, high = 0.5),
    "R1" = c(low = -0.002, high = 0.012),
    "R2" = c(low = -0.002, high = 0.01)
  ),
  "Roots 13C atom%" = list(
    "R0" = c(low = -0.005, high = 0.035),
    "R1" = c(low = -0.002, high = 0.01),
    "R2" = c(low = -0.002, high = 0.012)
  ),
  # --- New soil data limits ---
  "0-15 Soil13C atom%" = list(
    "R0" = c(low = -0.0005, high = 0.0025),
    "R1" = c(low = 0.0, high = 0.005),
    "R2" = c(low = 0.0, high = 0.005)
  ),
  "15-30 Soil 13C atom%" = list(
    "R0" = c(low = -0.0005, high = 0.0025),
    "R1" = c(low = 0.0, high = 0.003),
    "R2" = c(low = 0.0, high = 0.003)
  )
)

for (ramet_type in ramet_types) {
  current_ramet_data <- carbonFluxData %>%
    filter(Ramets == ramet_type)
  
  unique_times_current <- sort(unique(current_ramet_data$`sample time(d)`))
  current_ramet_data <- current_ramet_data %>%
    mutate(time_seq = match(`sample time(d)`, unique_times_current))
  
  # Split configs into tissue and soil
  tissue_configs <- list(
    list(y_var = "Leave 13C atom%", title_suffix = "leaves", y_lab = "Leave 13C atom%"),
    list(y_var = "Branches 13C atom%", title_suffix = "branches", y_lab = "Branches 13C atom%"),
    list(y_var = "Roots 13C atom%", title_suffix = "roots", y_lab = "Roots 13C atom%")
  )
  
  soil_configs <- list(
    list(y_var = "0-15 Soil13C atom%", title_suffix = "0-15cm soil", y_lab = "0-15cm Soil 13C atom%"),
    list(y_var = "15-30 Soil 13C atom%", title_suffix = "15-30cm soil", y_lab = "15-30cm Soil 13C atom%")
  )
  
  # Create tissue plots
  tissue_plots <- list()
  for (config in tissue_configs) {
    y_limits <- y_limits_config[[config$y_var]][[ramet_type]]
    y_low <- y_limits["low"]
    y_high <- y_limits["high"]
    
    plot_title <- paste0(ramet_type, " ramets - ", config$title_suffix)
    p <- create_plot(
      data = current_ramet_data,
      y_variable = config$y_var,
      plot_title = plot_title,
      y_label = config$y_lab,
      unique_times = unique_times_current,
      y_lim_low = y_low,
      y_lim_high = y_high
    )
    tissue_plots[[length(tissue_plots) + 1]] <- p
  }
  
  # Create soil plots
  soil_plots <- list()
  for (config in soil_configs) {
    y_limits <- y_limits_config[[config$y_var]][[ramet_type]]
    y_low <- y_limits["low"]
    y_high <- y_limits["high"]
    
    plot_title <- paste0(ramet_type, " ramets - ", config$title_suffix)
    p <- create_plot(
      data = current_ramet_data,
      y_variable = config$y_var,
      plot_title = plot_title,
      y_label = config$y_lab,
      unique_times = unique_times_current,
      y_lim_low = y_low,
      y_lim_high = y_high
    )
    soil_plots[[length(soil_plots) + 1]] <- p
  }
  
  # Combine into separate plot groups
  combined_tissue_plot <- wrap_plots(tissue_plots, ncol = 1) +
    plot_annotation(
      title = paste0("13C Atom% ", ramet_type, " - Plant Tissues")
    ) & theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  
  combined_soil_plot <- wrap_plots(soil_plots, ncol = 1) +
    plot_annotation(
      title = paste0("13C Atom% ", ramet_type, " - Soil")
    ) & theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  
  plot_list[[paste0(ramet_type, "_tissues")]] <- combined_tissue_plot
  plot_list[[paste0(ramet_type, "_soil")]] <- combined_soil_plot
}

# Print all tissue and soil plots separately
for (ramet_type in ramet_types) {
  print(plot_list[[paste0(ramet_type, "_tissues")]])
  print(plot_list[[paste0(ramet_type, "_soil")]])
}
