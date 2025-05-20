library(dplyr) # Optional, for data manipulation if needed
library(stringr)
library(scales)
library(readr)
library(lubridate)
library(stargazer)
library(tidyr)
library(stringr)
library(tidyverse)
library(purrr)
library(xtable)
library(igraph)
library(ggraph)
library(tidygraph)

get_surname <- function(x) str_trim(word(x, -1)) 

data_clean <- read_rds("data_clean.rds") %>%
  mutate(judge_name = get_surname(judge_name))
data_basic <- read_rds("data_basic.rds") %>%
  mutate(judge_name = get_surname(judge_name))

judge_pairs <- data_basic %>%
  select(doc_id, judge_name, composition_ok) %>%
  group_by(doc_id) %>%
  filter(n() == 3) %>%  # ensure only cases with 3 judges
  summarise(pairs = list(as_tibble(t(combn(judge_name, 2)))), .groups = "drop", composition_ok=mean(composition_ok)) %>%
  unnest(pairs) %>%
  rename(judge1 = V1, judge2 = V2)

edge_weights <- judge_pairs %>%
  rowwise() %>%
  mutate(
    j1 = min(judge1, judge2),
    j2 = max(judge1, judge2)
  ) %>%
  ungroup() %>%
  group_by(j1, j2) %>%
  summarise(
    n_cases_together = n(),
    prop_composition_ok = mean(composition_ok),  # share of cases with ok composition
    .groups = "drop"
  )



g <- tbl_graph(edges = edge_weights, directed = FALSE)

# Plot
ggraph(g, layout = "stress") +
  geom_edge_link(aes(width = n_cases_together, color = prop_composition_ok), alpha = 0.5) +
  geom_node_point(size = 4, color = "black") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_color_gradient2(low = "red", mid = "gray", high = "green", midpoint = 0.5) +
  scale_edge_width(range = c(0.5, 3)) +
  theme_void() +
  ggtitle("Judicial Co-decision Network (Edge color = % Composition OK)")



judge_pairs_ok <- data_clean %>%
  select(doc_id, judge_name, composition_ok) %>%
  group_by(doc_id) %>%
  filter(n() == 3) %>%  # ensure only cases with 3 judges
  summarise(pairs = list(as_tibble(t(combn(judge_name, 2)))), .groups = "drop", composition_ok=mean(composition_ok)) %>%
  unnest(pairs) %>%
  rename(judge1 = V1, judge2 = V2)

edge_weights_ok <- judge_pairs_ok %>%
  rowwise() %>%
  mutate(
    j1 = min(judge1, judge2),
    j2 = max(judge1, judge2)
  ) %>%
  ungroup() %>%
  group_by(j1, j2) %>%
  summarise(
    n_cases_together = n(),
    prop_composition_ok = mean(composition_ok),  # share of cases with ok composition
    .groups = "drop"
  )
g <- tbl_graph(edges = edge_weights_ok , directed = FALSE)

# Plot
ggraph(g, layout = "stress") +
  geom_edge_link(aes(width = n_cases_together), alpha = 0.5) +
  geom_node_point(size = 4, color = "black") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.5, 3)) +
  theme_void() +
  ggtitle("Judicial Co-decision Network")