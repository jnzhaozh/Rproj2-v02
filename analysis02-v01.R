pacman::p_load(igraph, tidyverse, forcats, furrr, here)

source(here("Rproj", "visualization.R"))

all_iterations <- readRDS(here("Rproj", "all_iterations_01.rds"))

alignment_dat_avg <- all_iterations %>%
  map("alignment_dat") %>%
  bind_rows(.id = "iteration") %>%
  filter(network_type == "p_medium") %>%
  group_by(topic_n, interest_type) %>%
  summarise(
    prop_aligned_edges = mean(prop_aligned_edges),
    prop_aligned_adopters = mean(prop_aligned_adopters),
    .groups = "drop"
  )

plot_alignment_dat <- alignment_dat_avg %>%
  pivot_longer(cols = starts_with("prop"),
               names_to = "pair_type",
               values_to = "proportion") %>%
  mutate(
    interest_type = factor(interest_type, levels = rev(
      c("cross_rare", "cross_few", "cross_some")
    )),
    pair_type = factor(
      pair_type,
      levels = c("prop_aligned_edges", "prop_aligned_adopters")
    )
  )


plot_social_influence <- ggplot(
  plot_alignment_dat,
  aes(
    x = topic_n,
    y = proportion,
    linetype = pair_type,
    color = interest_type,
    group = interaction(pair_type, interest_type)
  )
) +
  geom_line(size = .6) +
  geom_point(size = 1.2) +
  scale_color_manual(
    values = c(
      cross_rare = "#4477AA",
      cross_few  = "#009944",
      cross_some = "#CC3311"
    ),
    labels = c(
      cross_rare = "39.40% ≥ 2 interests",
      cross_few  = "61.29% ≥ 2 interests",
      cross_some = "80.95% ≥ 2 interests"
    ),
    name = "Interest Diversity"
  ) +
  scale_linetype_manual(
    values = c(
      "prop_aligned_edges"     = "solid",
      "prop_aligned_adopters"  = "dashed"
    ),
    labels = c(
      "prop_aligned_edges"     = "Potential Influence Ties",
      "prop_aligned_adopters"  = "Active Influence Ties"
    ),
    name = "Social Influence Ties"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(x = "Number of Topics", y = "Percentage of Network Connections") +
  guides(
    linetype = guide_legend(
      nrow = 2,
      title.position = "top",
      order = 1
    ),
    color = guide_legend(
      nrow = 3,
      title.position = "top",
      order = 2
    )
  ) +
  base_theme


ggsave(
  file.path("~/PhD-Rproj-01/figures/", "2_social_influence.pdf"),
  plot_social_influence,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300,
  device = cairo_pdf
)

