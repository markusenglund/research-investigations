library(ggplot2)
library(dplyr)
library(readxl)
library(patchwork)

# Load data
carbonFluxData <- read_excel("Downloads/Data_Dryad_Drought_Decreases_Carbon_Flux_but_Not_Transport_Speed_of_Newly_Fixed_Carbon_from_Leaves_to_Sinks_in_a_Giant_Bamboo_Forest.xlsx",
                             sheet = "Graph data")

# Filter for R0 data and prepare time sequence
r0_data <- carbonFluxData %>%
  filter(Ramets == "R0")

unique_times <- sort(unique(r0_data$`sample time(d)`))

r0_data <- r0_data %>%
  mutate(time_seq = match(`sample time(d)`, unique_times))

# --- Function to create a plot ---
create_r0_plot <- function(data, y_variable, plot_title, y_label) {
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
    theme(plot.title = element_text(hjust = 0.5))
}

# --- Create plots using the function ---
plot_r0_leaves <- create_r0_plot(
  data = r0_data,
  y_variable = "Leave 13C atom%",
  plot_title = "R0 ramets - leaves",
  y_label = "Leave 13C atom%"
)

plot_r0_branches <- create_r0_plot(
  data = r0_data,
  y_variable = "Branches 13C atom%",
  plot_title = "R0 Ramets - branches",
  y_label = "Branches 13C atom%"
)

plot_r0_roots <- create_r0_plot(
  data = r0_data,
  y_variable = "Roots 13C atom%",
  plot_title = "R0 Ramets - roots",
  y_label = "Roots 13C atom%"
)

# --- Combine and display the plots ---
combined_plots <- plot_r0_leaves / plot_r0_branches / plot_r0_roots +
  plot_annotation(
    title = "13C Atom% R0 Leaves, Branches, and Roots"
  ) & theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

print(combined_plots)