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
    end_date = dmy(end_date)
  )

# Create a function to find initial colleagues
find_initial_colleagues <- function(judge_row, full_data) {
  judge_start <- judge_row$start_date
  judge_chamber <- judge_row$chamber_id
  
  colleagues <- full_data %>%
    filter(
      chamber_id == judge_chamber,
      !is.na(judge_id),
      start_date < judge_start,
      end_date >= judge_start
    ) %>%
    arrange(start_date) %>%
    head(2)
  
  if (nrow(colleagues) == 0) return(NA)
  
  return(c(colleagues$judge_id))
}

# Apply the function
initial <- chambers %>%
  filter(!is.na(judge_id)) %>%
  rowwise() %>%
  mutate(initial_colleagues = list(find_initial_colleagues(cur_data(), chambers))) %>%
  ungroup() %>%
  select(judge_id, judge_name, start_date, chamber_id, initial_colleagues) %>%
  group_by(judge_id) %>%
  filter(start_date == min(start_date)) %>%
  ungroup()

ccc_judges <- read_rds("../data/rds/ccc_judges.rds")


judges <- ccc_judges %>% group_by(judge_id) %>%
  summarise(judge_term_start=min(ymd(judge_term_start)),
            judge_term_end=max(ymd(judge_term_end)),
            judge_reelection=max(judge_reelection)) %>%
  ungroup() %>%
  left_join(ccc_judges %>% select(judge_id,
                                  judge_yob,judge_gender, judge_uni, judge_degree, judge_profession), by=join_by(judge_id),multiple="first")

initial<- initial %>%
  unnest_longer(initial_colleagues, values_to = "colleague_id")

initial <- initial %>%
  left_join(judges, by = c("colleague_id" = "judge_id")) 

initial <- initial %>% group_by(judge_id) %>%
  summarise(average_yob=mean(judge_yob),
         var_yob=var(judge_yob),
         background = paste0(sort(str_sub(judge_profession, 1, 1)), collapse = ""),
         uni = paste0(sort(str_sub(judge_uni, 1, 1)), collapse = ""),
         gender = paste0(sort(str_sub(judge_gender, 1, 1)), collapse = ""),
         distinct_backgrounds=length(unique(judge_profession)),
         scholar=(any(judge_profession=="scholar")),
         distinct_uni=length(unique(judge_uni)))

saveRDS(initial, "initial.rds")

