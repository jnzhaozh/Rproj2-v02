pacman::p_load(igraph, tidyverse, forcats, furrr)

source(here("Rproj", "visualization.R"))

all_iterations <- readRDS(here("Rproj", "all_iterations_01.rds"))

adoption_dat_avg <- all_iterations %>%
  map("adoption_dat") %>%
  bind_rows(., .id = "iteration") %>%
  filter(network_type == "p_low") %>%
  group_by(topic_n, interest_n, interest_type) %>%
  summarise(
    audience_n = mean(audience_n),
    adopter_n = mean(adopter_n),
    .groups = "drop"
  )

plot_adoption_dat <- adoption_dat_avg %>%
  filter(interest_n > 0) %>%
  pivot_longer(
    cols = c(audience_n, adopter_n),
    names_to = "agent_type",
    values_to = "agent_n"
  ) %>%
  mutate(
    agent_type = factor(agent_type, levels = c("audience_n", "adopter_n")),
    interest_type = factor(interest_type, levels = rev(
      c("cross_rare", "cross_few", "cross_some")
    ))
  ) %>%
  summarise(
    agent_prop = sum(agent_n) / agents,
    .by = c(topic_n, agent_type, interest_type)
  )


plot_adoption_status <- ggplot(
  data = plot_adoption_dat,
  aes(
    x = topic_n,
    y = agent_prop,
    color = interest_type,
    linetype = agent_type,
    group = interaction(interest_type, agent_type)
  )
) +
  geom_line(size = 0.8) +
  geom_point(size = 1.8) +
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
    )
  ) +
  scale_linetype_manual(
    values = c(audience_n = "solid", adopter_n  = "dashed"),
    labels = c(audience_n = "Potential Audience", adopter_n  = "Actual Adopters")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Number of Topics",
    y = "Percentage of Population",
    color = "Interest Diversity",
    linetype = "Adoption Status"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,
      title.position = "top",
      order = 2
    ),
    linetype = guide_legend(
      nrow = 2,
      title.position = "top",
      order = 1
    )
  ) +
  base_theme


