###############################################################################
## Unified animated-network plots for three data sets
## -- copy-paste the whole file and run ----------------------------------------
###############################################################################

## ---- 0. Libraries -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(tibble)
library(data.table)

library(igraph)
library(tidygraph)
library(ggraph)

library(ggplot2)
library(gganimate)
library(gifski)

## ---- 1. Helper: keep only the surname ---------------------------------------
get_surname <- function(x) str_trim(word(x, -1))   # “John Van Doe” → “Doe”

## ---- 2. Global design constants ---------------------------------------------
base_theme <- theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "bottom")

node_point_size  <- 3
node_text_size   <- 5
edge_alpha       <- 0.40
edge_width_range <- c(0.3, 2)
node_color       <- "black"
edge_gray        <- "gray50"

## ---- 3. BASIC data set -------------------------------------------------------
data_basic <- read_rds("data_basic.rds") %>%
  mutate(judge_name = get_surname(judge_name))

judge_pairs_time <- data_basic %>%
  select(doc_id, judge_name, composition_ok, date_submission) %>%
  mutate(month = floor_date(date_submission, "month")) %>%
  group_by(doc_id) %>%
  filter(n() == 3) %>%
  summarise(
    pairs          = list(as_tibble(t(combn(judge_name, 2)))),
    month          = first(month),
    composition_ok = mean(composition_ok),
    .groups        = "drop"
  ) %>%
  unnest(pairs) %>%
  rename(judge1 = V1, judge2 = V2)
saveRDS(judge_pairs_time, "judge_pairs_time.rds")
all_months <- sort(unique(judge_pairs_time$month))

saveRDS(all_months, "all_months.rds")

edges_cumulative <- map_dfr(all_months, function(m) {
  judge_pairs_time %>%
    filter(month <= m) %>%
    rowwise() %>%
    mutate(j1 = min(judge1, judge2),
           j2 = max(judge1, judge2)) %>%
    ungroup() %>%
    group_by(j1, j2) %>%
    summarise(n_cases_together = n(),
              prop_ok          = mean(composition_ok),
              .groups          = "drop") %>%
    mutate(month = m)
})
saveRDS(edges_cumulative, "edges_cumulative.rds")



nodes_basic <- tibble(name = unique(c(edges_cumulative$j1, edges_cumulative$j2)))

g_basic     <- tbl_graph(nodes = nodes_basic, edges = edges_cumulative, directed = FALSE)
layout_basic <- create_layout(g_basic, layout = "stress")


p_basic <- ggraph(layout_basic) +
  geom_edge_link(aes(width = n_cases_together, color = prop_ok),
                 alpha = edge_alpha) +
  geom_node_point(size = node_point_size, color = node_color) +
  geom_node_text(aes(label = name), size = node_text_size, vjust = 2, hjust=0.45) +
  scale_edge_width(range = edge_width_range) +
  base_theme +
  labs(title = "Judicial Co-decision Network (BASIC): {frame_time}") +
  transition_time(month) +
  ease_aes("linear") +
  guides(color = guide_colorbar(title.position = "top"),
         width = guide_legend(title.position = "top"))

animate(p_basic,
        nframes   = length(all_months),
        fps       = 2,
        width     = 800,
        height    = 600,
        renderer  = gifski_renderer("network_basic.gif"))

## ---- 4. CLEAN data set -------------------------------------------------------
data_clean <- read_rds("data_clean.rds") %>%
  mutate(judge_name = get_surname(judge_name))

judge_pairs_time_clean <- data_clean %>%
  select(doc_id, judge_name, composition_ok, date_submission) %>%
  mutate(month = floor_date(date_submission, "month")) %>%
  group_by(doc_id) %>%
  filter(n() == 3) %>%
  summarise(
    pairs  = list(as_tibble(t(combn(judge_name, 2)))),
    month  = first(month),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  rename(judge1 = V1, judge2 = V2)

all_months_clean <- sort(unique(judge_pairs_time_clean$month))
saveRDS(all_months_clean, "all_months_clean.rds")


edges_cumulative_clean <- map_dfr(all_months_clean, function(m) {
  judge_pairs_time_clean %>%
    filter(month <= m) %>%
    rowwise() %>%
    mutate(j1 = min(judge1, judge2),
           j2 = max(judge1, judge2)) %>%
    ungroup() %>%
    group_by(j1, j2) %>%
    summarise(n_cases_together = n(), .groups = "drop") %>%
    mutate(month = m)
})
saveRDS(edges_cumulative_clean, "edges_cumulative_clean.rds")

nodes_clean  <- tibble(name = unique(c(edges_cumulative_clean$j1, edges_cumulative_clean$j2)))
g_clean      <- tbl_graph(nodes = nodes_clean, edges = edges_cumulative_clean, directed = FALSE)
layout_clean <- create_layout(g_clean, layout = "stress")
saveRDS(layout_clean, "layout_clean.rds")


p_clean <- ggraph(layout_clean) +
  geom_edge_link(aes(width = n_cases_together),
                 alpha = edge_alpha, color = edge_gray) +
  geom_node_point(size = node_point_size, color = node_color) +
  geom_node_text(aes(label = name), size = node_text_size, vjust = 2, hjust=0.45) +
  scale_edge_width(range = edge_width_range) +
  base_theme +
  labs(title = "Judicial Co-decision Network (CLEAN): {frame_time}") +
  transition_time(month) +
  ease_aes("linear") +
  guides(width = guide_legend(title.position = "top"))

animate(p_clean,
        nframes   = length(all_months_clean),
        fps       = 2,
        width     = 800,
        height    = 600,
        renderer  = gifski_renderer("network_clean.gif"))

## ---- 5. ASSIGNED / Chamber composition --------------------------------------
chambers <- read.csv("../data/csv/chamber compositions.csv") %>%
  mutate(judge_name = get_surname(judge_name),
         start_date = dmy(start_date),
         end_date   = dmy(end_date)) %>%
  mutate(end_date = coalesce(end_date, dmy("31/12/2024")),
         chamber_number = str_trim(chamber_id))

chambers_monthly <- chambers %>%
  mutate(date_seq = map2(start_date, end_date,
                         ~ seq(from = floor_date(.x, "month"),
                               to   = floor_date(.y, "month"),
                               by   = "month"))) %>%
  unnest(date_seq)

ani_start <- as_date("2016-01-01")

edges_monthly <- chambers_monthly %>%
  filter(date_seq >= ani_start,
         judge_name != "unappointed",
         !is.na(judge_name)) %>%
  group_by(date = date_seq, chamber = chamber_number) %>%
  summarise(
    pairs = if (n_distinct(judge_name) >= 2)
      list(combn(unique(judge_name), 2, simplify = FALSE))
    else list(NULL),
    .groups = "drop") %>%
  unnest(pairs) %>%
  mutate(from = map_chr(pairs, 1),
         to   = map_chr(pairs, 2)) %>%
  select(date, from, to)

layout_graph <- graph_from_data_frame(distinct(edges_monthly, from, to), directed = FALSE)
coords <- as.data.frame(layout_with_fr(layout_graph))
coords$judge_name <- V(layout_graph)$name
names(coords)[1:2] <- c("x", "y")

edges_plot <- edges_monthly %>%
  left_join(coords,  by = c("from" = "judge_name")) %>%
  rename(x_start = x, y_start = y) %>%
  left_join(coords,  by = c("to"   = "judge_name")) %>%
  rename(x_end   = x, y_end   = y)

nodes_plot <- tidyr::crossing(coords, date = unique(edges_monthly$date))

p_assigned <- ggplot() +
  geom_segment(data = edges_plot,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
                   group = interaction(from, to)),
               alpha = edge_alpha, color = edge_gray) +
  geom_point(data = nodes_plot,
             aes(x = x, y = y),
             size = node_point_size, color = "steelblue") +
  geom_text(data = nodes_plot,
            aes(x = x, y = y, label = judge_name),
            vjust = 2, hjust=0.45, size = 3) +
  base_theme +
  labs(title = "Chamber Network: {frame_time}") +
  transition_time(date) +
  ease_aes("linear")

animate(p_assigned,
        fps       = 2,
        width     = 800,
        height    = 600,
        renderer  = gifski_renderer("network_assigned.gif"))
