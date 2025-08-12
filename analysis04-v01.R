pacman::p_load(igraph, tidyverse, forcats, furrr, here)

source(here("upload", "visualization.R"))
source(here("upload", "simulation-v01.R"))
heatmap_data <- readRDS(here("upload", "heatmap_data_01.rds"))


agents <-  2500
topics <-  5
iterations <- 50
param_grid <- cross_df(list(
  connectivity = round((1:15) / (agents - 1), 7),
  alignment = seq(1, 15, by = 1)
))

# -------------------------------------------------------------------------

run_diffusion_simulation_fixed <- function(interest_mat, sample_net, seed_fraction = 0.1) {
  agents <- nrow(interest_mat)
  topics <- ncol(interest_mat)
  
  # Generate thresholds
  threshold <- apply(interest_mat, 2, function(x) {
    x[x == 1] <- runif(sum(x), min = 1e-10, max = 1 - 1e-10)
    x
  }) %>% split(., row(.))
  
  # Generate influence weights
  influence <- apply(interest_mat, 2, function(x) {
    x[x == 1] <- runif(sum(x), min = 1e-10, max = 1 - 1e-10)
    x
  }) %>% split(., row(.))
  
  # Initialize adoption status
  initial_status <- integer(agents)
  initial_status[sample(agents, size = floor(seed_fraction * agents))] <- 1
  
  neighbor_lists <- adjacent_vertices(sample_net, V(sample_net), mode = "in")
  
  topic_n <- topics
  status <- initial_status
  changed <- TRUE
  
  while (changed) {
    changed <- FALSE
    
    for (i in seq_along(status)) {
      if (status[i] != 0)
        next
      
      neighbors_list <- neighbor_lists[[i]]
      if (length(neighbors_list) == 0)
        next
      
      neighbors_influence <- numeric(topics)
      
      for (j in neighbors_list) {
        neighbors_influence <- neighbors_influence + influence[[j]] * status[j]
      }
      
      threshold_i <- threshold[[i]][1:topic_n]
      neighbors_influence_i <- neighbors_influence[1:topic_n]
      interests_i <- which(threshold_i != 0)
      
      if (length(interests_i) > 0) {
        activated <- any(neighbors_influence_i[interests_i] >= threshold_i[interests_i])
        if (activated) {
          status[i] <- 1
          changed <- TRUE
        }
      }
    }
  }
  
  return(list(interest_mat = interest_mat, status_vector = status))
}


# -------------------------------------------------------------------------


plan(multisession)
tictoc::tic()

heatmap_data <- future_pmap_dfr(
  .l = list(p = param_grid$connectivity, beta = param_grid$alignment),
  .f = function(p, beta) {
    adoption_rates <- replicate(iterations, {
      interest_mat <- generate_topic_interest(agents, topics, alpha = 1, beta)
      sample_net <- generate_sample_network(agents, p)
      result <- run_diffusion_simulation_fixed(interest_mat, sample_net)
      adoption_rate <- mean(result$status_vector)
      
    })
    
    tibble(
      connectivity = p,
      alignment = beta,
      avg_adoption_rate = mean(adoption_rates),
      sd_adoption_rate = sd(adoption_rates)
    )
  },
  .options = furrr_options(seed = TRUE)
)

tictoc::toc()
plan(sequential)

# -------------------------------------------------------------------------


plot_heatmap <- ggplot(heatmap_data,
                       aes(x = connectivity, y = alignment, fill = avg_adoption_rate)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    limits = c(0, 1),
    breaks = c(0, 0.5, 1.0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(breaks = unique(heatmap_data$connectivity),
                     # labels = scales::number_format(accuracy = 0.0001)
                     labels = \(x) round(x * 1000, 1)) +
  scale_y_continuous(breaks = unique(heatmap_data$alignment)) +
  labs(x = "Edge Probability p (\u2030)", y = "Interest Diversity Shape β", fill = "Avg. Adoption Rate") +
  coord_cartesian(
    xlim = range(heatmap_data$connectivity),
    ylim = range(heatmap_data$alignment)
  ) +
  guides(fill = guide_colorbar(title.position = "top", barwidth = unit(5, "cm"))) +
  base_theme +
  theme(legend.position = "bottom", legend.direction = "horizontal")
