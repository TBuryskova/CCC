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
library(data.table)

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
  scale_edge_width(range = c(0.3, 2)) +
  theme_void() +
  labs(title = "Judicial Co-decision Network: {frame_time}") +
  transition_time(month) +
  ease_aes("linear")

animate(p, nframes = length(all_months), fps = 2, width = 800, height = 600,
        renderer = gifski_renderer("chamber_basic.gif"))

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
        renderer = gifski_renderer("chamber_clean.gif"))

#----------assigned#



chambers <- read.csv("../data/csv/chamber compositions.csv")


chambers <- chambers %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)) %>% 
  mutate(end_date=case_when(is.na(end_date)~ dmy("31/12/2024"),
                            TRUE ~ end_date) )%>%
  rename(chamber_number=chamber_id)  %>%
  mutate(chamber_number = str_trim(chamber_number))

chambers_monthly <- chambers %>%
  mutate(
    date_seq = map2(start_date, end_date, ~ seq(from = floor_date(.x, "month"),
                                                to = floor_date(.y, "month"),
                                                by = "month"))
  ) %>%
  unnest(date_seq)

## 6 ── label monthly chamber composition --------------------------------------
monthly <- chambers_monthly %>%
  transmute(date = date_seq,
            chamber = chamber_number,
            judge_name = na_if(judge_name, "unappointed")) %>%
  group_by(date, chamber) %>%
  arrange(judge_name, .by_group = TRUE) %>%
  mutate(judge_pos = paste0("judge", row_number())) %>%
  pivot_wider(names_from = judge_pos,
              values_from = judge_name) %>%
  ungroup()

## 7 ── collapse consecutive identical periods -----------------------
setDT(monthly)[order(chamber, date)]
monthly[, grp := rleid(chamber, judge1, judge2, judge3)]

chambers_final <- monthly[, 
                          .(date_start = min(date),
                            date_end = max(date),
                            judge1 = judge1[1],
                            judge2 = judge2[1],
                            judge3 = judge3[1]),
                          by = .(chamber, grp)][, grp := NULL][]

ani_start <- as_date("2016-01-01")

# A. build an edge list for every 1st of month ≥ ani_start --------------------
edges_monthly <- chambers_monthly %>%
  filter(date_seq >= ani_start) %>%
  filter(judge_name != "unappointed" & !is.na(judge_name)) %>%
  group_by(date = date_seq, chamber = chamber_number) %>%
  summarise(
    pairs = if (n_distinct(judge_name) >= 2) {
      list(combn(unique(judge_name), 2, simplify = FALSE))
    } else {
      list(NULL)
    },
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  mutate(
    from = map_chr(pairs, 1),
    to = map_chr(pairs, 2)
  ) %>%
  select(date, from, to)

# B. stable layout: use *all* edges in 2016‑24 period -----------------------
layout_graph <- graph_from_data_frame(distinct(edges_monthly, from, to), directed = FALSE)
coords <- as.data.frame(layout_with_fr(layout_graph))
coords$judge_name <- V(layout_graph)$name
names(coords)[1:2] <- c("x", "y")

# C. prepare edge & node frames --------------------------------------------
edges_plot <- edges_monthly %>%
  left_join(coords, by = c("from" = "judge_name")) %>%
  rename(x_start = x, y_start = y) %>%
  left_join(coords, by = c("to" = "judge_name")) %>%
  rename(x_end = x, y_end = y)

nodes_plot <- tidyr::crossing(coords, date = unique(edges_monthly$date))

# D. build animated plot ----------------------------------------------------
p <- ggplot() +
  geom_segment(data = edges_plot,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = interaction(from, to)),
               alpha = 0.4) +
  geom_point(data = nodes_plot,
             aes(x = x, y = y), size = 3, colour = "steelblue") +
  geom_text(data = nodes_plot,
            aes(x = x, y = y, label = judge_name), vjust = 1.5, size = 3) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  transition_time(date) +
  labs(title = 'Chamber network — {frame_time}')

# E. render to GIF ----------------------------------------------------------
animate(p, renderer = gifski_renderer("assigned.gif"),
                width = 800, height =600, fps = 2)