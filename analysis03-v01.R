pacman::p_load(igraph, tidyverse, forcats, furrr)

source(here("Rproj", "visualization.R"))

all_iterations <- readRDS(here("Rproj", "all_iterations_01.rds"))

alignment_dat_avg <- all_iterations %>%
  map("alignment_dat") %>%
  bind_rows(.id = "iteration") %>%
  filter(network_type %in% c("p_low", "p_medium", "p_high")) %>%
  group_by(topic_n, interest_type, network_type) %>%
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
    ),
    network_type = factor(network_type, levels = c("p_low", "p_medium", "p_high"))
  )


plot_social_network <- ggplot(
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
  facet_wrap(. ~ network_type, labeller = labeller(
    network_type = c(
      p_low = "Network Avg. Degree ≈ 3",
      p_medium = "Network Avg. Degree ≈ 6",
      p_high = "Network Avg. Degree ≈ 12"
    )
  )) +
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
      "prop_aligned_edges" = "solid",
      "prop_aligned_adopters" = "dashed"
    ),
    labels = c(
      "prop_aligned_edges" = "Potential Influence Ties",
      "prop_aligned_adopters" = "Active Influence Ties"
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

