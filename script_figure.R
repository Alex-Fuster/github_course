library(ggplot2)
library(dplyr)

# Start with root species at center
species <- data.frame(
  id = 1,
  parent = NA,
  x = 0.5,
  y = 0.5,
  gen = 0
)

# Parameters
n_generations <- 3
branching_prob <- 0.5
step_sd <- 0.06
next_id <- 2

for (g in 1:n_generations) {
  current_gen <- species %>% filter(gen == g - 1)
  for (i in 1:nrow(current_gen)) {
    n_offspring <- 1 + rbinom(1, 1, branching_prob)
    for (k in 1:n_offspring) {
      new_x <- current_gen$x[i] + rnorm(1, 0, step_sd)
      new_y <- current_gen$y[i] + rnorm(1, 0, step_sd)
      species <- rbind(
        species,
        data.frame(
          id = next_id,
          parent = current_gen$id[i],
          x = new_x,
          y = new_y,
          gen = g
        )
      )
      next_id <- next_id + 1
    }
  }
}


# Create edges
edges <- species %>%
  filter(!is.na(parent)) %>%
  left_join(species, by = c("parent" = "id"), suffix = c("", ".parent"))

# Plot
ggplot() +
  # background
  theme_minimal(base_size = 14) +
  coord_fixed(xlim = c(0,1), ylim = c(0,1)) +
  
  # edges
  geom_segment(
    data = edges,
    aes(x = x.parent, y = y.parent, xend = x, yend = y),
    color = "grey50",
    size = 0.7
  ) +
  
  # nodes
  geom_point(
    data = species,
    aes(x = x, y = y, fill = factor(gen)),
    shape = 21, color = "black", size = 4, alpha = 0.8
  ) +
  
  # styling
  scale_fill_viridis_d(name = "Generation") +
  labs(
    title = "Illustrative Niche Filling in Trait Space",
    x = "Trait 1",
    y = "Trait 2"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )