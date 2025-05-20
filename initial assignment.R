library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)

# Load your dataset
chambers <- read.csv("../data/csv/chamber compositions.csv")

# Convert date columns
chambers <- chambers %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)) %>% 
  mutate(end_date=case_when(is.na(end_date)~ dmy("31/12/2024"),
                            TRUE ~ end_date) )%>%
  rename(formation=chamber_id) %>%
  rename(asjudge_id=judge_id) %>%
  mutate(formation = str_trim(formation))

chambers_daily <- chambers %>%
  mutate(date_seq = map2(start_date, end_date,
                         ~ seq(.x, .y, by = "day"))) %>%
  unnest(date_seq) %>% select(-start_date,-end_date) 


judges <- ccc_judges %>% group_by(judge_id) %>%
  summarise(judge_term_start=(min(ymd(judge_term_start))),
            judge_term_end=ymd(max(ymd(judge_term_end))),
            judge_reelection=max(judge_reelection)) %>%
  ungroup() %>%
  left_join(ccc_judges %>% select(judge_id,judge_name,judge_yob,judge_gender, judge_uni, judge_degree, judge_profession), by=join_by(judge_id),multiple="first") %>%
  mutate(official1623=case_when(judge_id %in% c("J:41","J:40","J:42") ~ TRUE,
                                TRUE ~ FALSE)) %>%
  mutate(judge_id = str_trim(judge_id)) 

judge_init_formation <- chambers_daily %>% left_join(judges, by = c("asjudge_id"="judge_id")) %>%
  filter(date_seq==judge_term_start +days(20))


# 2. For each judge, find other judges in same formation on the same day
initial_colleagues <- judge_init_formation %>%
  left_join(chambers_daily, by = c("date_seq")) %>%
  filter(asjudge_id.x != asjudge_id.y) %>%  # remove self
  group_by(asjudge_id.x) %>%
  summarise(
    colleagues_at_entry = list(head(unique(asjudge_id.y), 2)),
    .groups = "drop"
  )

# 3. Join back to the original judges data

# Assume colleagues_df is your input data with asjudge_id.x and colleagues_at_entry
# and judges is the full dataset with judge_id as primary key

# Step 1: Unnest colleagues into individual rows
colleagues_long <- initial_colleagues%>%
  rename(judge_id = asjudge_id.x) %>%
  unnest_longer(colleagues_at_entry, values_to = "colleague_id")

# Step 2: Join to get colleague details
colleagues_with_details <- colleagues_long %>%
  left_join(judges, by = c("colleague_id" = "judge_id"))

# Step 3: Summarise statistics for each main judge_id
initial <- colleagues_with_details %>%
  group_by(judge_id) %>%
  summarise(
    average_yob = mean(judge_yob, na.rm = TRUE),
    var_yob = var(judge_yob, na.rm = TRUE),
    background = paste0(sort(str_sub(judge_profession, 1, 1)), collapse = ""),
    uni = paste0(sort(str_sub(judge_uni, 1, 1)), collapse = ""),
    gender = paste0(sort(str_sub(judge_gender, 1, 1)), collapse = ""),
    distinct_backgrounds = n_distinct(judge_profession),
    scholar = any(judge_profession == "scholar"),
    distinct_uni = n_distinct(judge_uni),
    .groups = "drop"
  )


saveRDS(initial, "initial.rds")

