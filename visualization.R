library(ggplot2)

base_layer <- list(geom_line(linewidth = .6), geom_point(size = 1.2))
# scale_y_continuous(labels = scales::percent_format(accuracy = .1))

base_theme <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(
    panel.background = element_rect(fill = "grey95", color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "grey95", color = NA)
  )