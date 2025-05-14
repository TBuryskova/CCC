library(dplyr)
library(stringr)
library(scales)
library(readr)
library(lubridate)
library(stargazer)
library(tidyr)
library(tidyverse)
library(purrr)
library(xtable)
library(igraph)
library(ggraph)
library(tidygraph)
library(gganimate)
library(gifski)

# ---------------------------- BASIC ----------------------------

data_clean <- read_rds("data_clean.rds")
data_basic <- read_rds("data_basic.rds") 

judge_pairs_time <- data_basic %>%
  select(doc_id, judge_name, composition_ok, date_submission) %>%
  mutate(month = floor_date(date_submission, "month")) %>%
  group_by(doc_id) %>%
  filter(n() == 3) %>%
  summarise(
    pairs = list(as_tibble(t(combn(judge_name, 2)))),
    month = first(month),
    composition_ok = mean(composition_ok),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  rename(judge1 = V1, judge2 = V2)

# All months sorted
all_months <- sort(unique(judge_pairs_time$month))

# Create cumulative edge data
edges_cumulative <- map_dfr(all_months, function(m) {
  judge_pairs_time %>%
    filter(month <= m) %>%
    rowwise() %>%
    mutate(j1 = min(judge1, judge2), j2 = max(judge1, judge2)) %>%
    ungroup() %>%
    group_by(j1, j2) %>%
    summarise(
      n_cases_together = n(),
      prop_ok = mean(composition_ok),
      .groups = "drop"
    ) %>%
    mutate(month = m)
})

all_nodes <- unique(c(edges_cumulative$j1, edges_cumulative$j2))
nodes_df <- tibble(name = all_nodes)

g_dynamic <- tbl_graph(nodes = nodes_df, edges = edges_cumulative, directed = FALSE)
layout_static <- create_layout(g_dynamic, layout = "stress")

p <- ggraph(layout_static) +
  geom_edge_link(aes(width = n_cases_together, color = prop_ok), alpha = 0.3) +
  geom_node_point(size = 3, color = "black") +
  geom_node_text(aes(label = name), size = 5, repel = FALSE) +
  scale_edge_color_gradient2(low = "red", mid = "gray", high = "green", midpoint = 0.5) +
  scale_edge_width(range = c(0.3, 2)) +
  theme_void() +
  labs(title = "Judicial Co-decision Network: {frame_time}") +
  transition_time(month) +
  ease_aes("linear")

animate(p, nframes = length(all_months), fps = 2, width = 800, height = 600,
        renderer = gifski_renderer("my_animation.gif"))

# ---------------------------- CLEAN ----------------------------

judge_pairs_time_clean <- data_clean %>%
  select(doc_id, judge_name, composition_ok, date_submission) %>%
  mutate(month = floor_date(date_submission, "month")) %>%
  group_by(doc_id) %>%
  filter(n() == 3) %>%
  summarise(
    pairs = list(as_tibble(t(combn(judge_name, 2)))),
    month = first(month),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  rename(judge1 = V1, judge2 = V2)

all_months_clean <- sort(unique(judge_pairs_time_clean$month))

edges_cumulative_clean <- map_dfr(all_months_clean, function(m) {
  judge_pairs_time_clean %>%
    filter(month <= m) %>%
    rowwise() %>%
    mutate(j1 = min(judge1, judge2), j2 = max(judge1, judge2)) %>%
    ungroup() %>%
    group_by(j1, j2) %>%
    summarise(n_cases_together = n(), .groups = "drop") %>%
    mutate(month = m)
})

all_nodes_clean <- unique(c(edges_cumulative_clean$j1, edges_cumulative_clean$j2))
nodes_df_clean <- tibble(name = all_nodes_clean)

g_dynamic_clean <- tbl_graph(nodes = nodes_df_clean, edges = edges_cumulative_clean, directed = FALSE)
layout_static_clean <- create_layout(g_dynamic_clean, layout = "stress")

p_clean <- ggraph(layout_static_clean) +
  geom_edge_link(aes(width = n_cases_together), alpha = 0.3) +
  geom_node_point(size = 3, color = "black") +
  geom_node_text(aes(label = name, x = x + 0.05, y = y + 0.05), size = 5) +
  scale_edge_width(range = c(0.3, 2)) +
  theme_void() +
  labs(title = "Judicial Co-decision Network: {frame_time}") +
  transition_time(month) +
  ease_aes("linear")

animate(p_clean, nframes = length(all_months_clean), fps = 2, width = 800, height = 600,
        renderer = gifski_renderer("my_animation_clean.gif"))
