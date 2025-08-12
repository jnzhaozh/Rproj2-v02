pacman::p_load(igraph, tidyverse, VGAM, forcats, furrr, here)

source(here("Rproj", "visualization.R"))

# -------------------------------------------------------------------------


generate_interest_matrix <- function(agents, topics, alpha, beta) {
  # k <- seq_len(topics)
  # log_probs <- lchoose(topics, k) + lbeta(alpha + k, beta + topics - k) - lbeta(alpha, beta)
  #
  # probabilities <- exp(log_probs) %>%
  #   {
  #     . / sum(.)
  #   }
  
  probabilities <- VGAM::dbetabinom.ab(0:topics,
                                       size = topics,
                                       shape1 = alpha,
                                       shape2 = beta) %>%
    {
      .[2:(topics + 1)] / (1 - .[1])
    }
  
  
  interest_n <- sample(
    x = seq_len(topics),
    size = agents,
    replace = TRUE,
    prob = probabilities
  )
  
  interest_list <- lapply(interest_n, \(n) {
    sample(seq_len(topics), size = n, replace = FALSE)
  })
  
  rows <- rep(seq_len(agents), times = interest_n)
  cols <- unlist(interest_list)
  
  interest_matrix <- Matrix::sparseMatrix(i = rows,
                                          j = cols,
                                          dims = c(agents, topics))
  
  as.matrix(interest_matrix)
  
}


# -------------------------------------------------------------------------


generate_sample_network <- function(agents, p) {
  network_func <- function(g) {
    while (any(degree(g) == 0)) {
      g <- add_edges(g, c(sample(V(g)[degree(g) == 0], 1), sample(V(g), 1)))
    }
    return(g)
  }
  
  network <- sample_gnp(
    n = agents,
    p = p,
    directed = FALSE,
    loops = FALSE
  ) %>%
    network_func()
  # as_directed(mode = "mutual")
  
  return(network)
}


# -------------------------------------------------------------------------

run_diffusion_simulation <- function(interest_mat, sample_net, seed_fraction = 0.1) {
  agents <- nrow(interest_mat)
  topics <- ncol(interest_mat)
  
  # Generate threshold
  threshold <- apply(interest_mat, 2, \(x) {
    x[x == 1] <- runif(sum(x), min = 1e-10, max = 1 - 1e-10)
    x
  }) %>% split(., row(.))
  
  # Generate influence
  influence <- apply(interest_mat, 2, \(x) {
    x[x == 1] <- runif(sum(x), min = 1e-10, max = 1 - 1e-10)
    x
  }) %>% split(., row(.))
  
  # Initialize adoption status
  initial_status <- replace(integer(agents), sample(agents, size = floor(seed_fraction * agents)), 1)
  
  # Run diffusion
  final_status <- map(seq_len(topics), \(topic_n) {
    status <- initial_status
    changed <- TRUE
    
    while (changed) {
      changed <- FALSE
      
      for (i in seq_along(status)) {
        if (status[i] != 0)
          next
        
        neighbors_list <- neighbors(sample_net, i, mode = "in")
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
          # activation_condition: any influence ≥ threshold
          activated <- any(neighbors_influence_i[interests_i] >= threshold_i[interests_i])
          
          if (activated) {
            status[i] <- 1
            changed <- TRUE
          }
        }
      }
    }
    
    return(status)
  })
  
  status_mat <- do.call(cbind, final_status)
  
  return(list(interest_mat = interest_mat, status_mat = status_mat))
}

run_multiple_diffusions <- function(interest_mat_list, sample_net) {
  result_list <- lapply(interest_mat_list, \(interest_mat) {
    run_diffusion_simulation(interest_mat, sample_net)
  })
  return(result_list)
}

# -------------------------------------------------------------------------

get_adoption_data <- function(diffusion_dat) {
  interest_mat <- diffusion_dat[["interest_mat"]]
  status_mat <- diffusion_dat[["status_mat"]]
  
  agents <- nrow(interest_mat)
  topics <- ncol(interest_mat)
  
  # For each topic_n
  topic_adoption <- map_dfr(seq_len(topics), \(topic_n) {
    # For each agent: how many interests among first topic_n topics
    interest_n_vec <- rowSums(interest_mat[, 1:topic_n, drop = FALSE])
    
    # Get adoption status for current topic_n
    adopters_vec <- status_mat[, topic_n]
    
    # Combine into tibble
    df <- tibble(
      agent_id = seq_len(agents),
      topic_n,
      interest_n = interest_n_vec,
      adopter = adopters_vec
    ) %>%
      group_by(topic_n, interest_n) %>%
      summarise(
        audience_n = n(),
        adopter_n = sum(adopter),
        .groups = "drop"
      ) %>%
      arrange(topic_n, interest_n)
    
    return(df)
  })
  
  return(topic_adoption)
}


# -------------------------------------------------------------------------

get_alignment_data <- function(interest_mat, status_mat, sample_net) {
  agents <- nrow(interest_mat)
  topics <- ncol(interest_mat)
  
  edge_list <- sample_net %>%
    igraph::as_data_frame(., what = "edges") %>%
    mutate(from = as.integer(from), to = as.integer(to))
  
  topic_alignment <- map_dfr(seq_len(topics), \(topic_n) {
    interest_sub <- interest_mat[, 1:topic_n, drop = FALSE]
    status_sub <- status_mat[, topic_n]
    
    # aligned_pairs <- interest_sub %*% t(interest_sub) %>%
    #   {
    #     sum((.)[lower.tri(.)] > 0)
    #   }
    
    aligned_edges <- edge_list %>%
      mutate(alignment_edges = map2_lgl(from, to, \(i, j) {
        any(interest_sub[i, ] * interest_sub[j, ] > 0)
      }))
    
    aligned_adopters <- edge_list %>%
      mutate(alignment_adopters = map2_lgl(from, to, \(i, j) {
        status_sub[i] == 1 && status_sub[j] == 1
      }))
    
    # tibble(
    #   topic_n = topic_n,
    #   prop_aligned_pairs = aligned_pairs / choose(agents, 2),
    #   prop_aligned_edges = sum(aligned_edges$alignment_edges) / ecount(sample_net),
    #   prop_aligned_adopters = ifelse(
    #     sum(aligned_edges$alignment_edges) == 0,
    #     0,
    #     sum(aligned_adopters$alignment_adopters) / sum(aligned_edges$alignment_edges)
    #   )
    # )
    
    tibble(
      topic_n = topic_n,
      prop_aligned_edges = sum(aligned_edges$alignment_edges) / ecount(sample_net),
      prop_aligned_adopters = sum(
        aligned_adopters$alignment_adopters & aligned_edges$alignment_edges
      ) / ecount(sample_net)
    )
  })
  
  return(topic_alignment)
}


# -------------------------------------------------------------------------


run_one_iteration <- function(agents, topics) {
  # Step 1: generate interest matrices once
  interest_mat_list <- list(
    cross_rare = generate_topic_interest(agents, topics, alpha = 1, beta = 10),
    cross_few = generate_topic_interest(agents, topics, alpha = 1, beta = 5),
    cross_some = generate_topic_interest(agents, topics, alpha = 1, beta = 2)
  )
  
  # Step 2: generate 3 NEW networks
  sample_net_list <- list(
    p_low = generate_sample_network(agents, p = .0012),
    p_medium = generate_sample_network(agents, p = .0024),
    p_high = generate_sample_network(agents, p = .0048)
  )
  
  # Step 3: loop over networks
  result_list <- lapply(names(sample_net_list), \(net_type) {
    sample_net <- sample_net_list[[net_type]]
    
    diffusion_dat <- run_multiple_diffusions(interest_mat_list, sample_net)
    
    adoption_dat <- bind_rows(lapply(names(diffusion_dat), \(cross_type) {
      get_adoption_data(diffusion_dat[[cross_type]]) %>%
        mutate(interest_type = cross_type, network_type = net_type)
    }))
    
    alignment_dat <- bind_rows(lapply(names(diffusion_dat), \(cross_type) {
      dat <- diffusion_dat[[cross_type]]
      get_alignment_data(dat$interest_mat, dat$status_mat, sample_net) %>%
        mutate(interest_type = cross_type, network_type = net_type)
    }))
    
    list(adoption_dat = adoption_dat, alignment_dat = alignment_dat)
  })
  
  # Combine all results into single big tables
  adoption_dat_all <- bind_rows(lapply(result_list, \(x) x$adoption_dat))
  alignment_dat_all <- bind_rows(lapply(result_list, \(x) x$alignment_dat))
  
  return(list(adoption_dat = adoption_dat_all, alignment_dat = alignment_dat_all))
}


# -------------------------------------------------------------------------

agents <- 2500
topics <- 5
iterations <- 50

tictoc::tic()
plan(multisession, workers = parallel::detectCores())

all_iterations <- future_map(1:iterations,
                             ~ run_one_iteration(agents, topics),
                             .options = furrr_options(seed = TRUE))

plan(sequential)
tictoc::toc()
