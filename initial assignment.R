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
  
  return(colleagues$judge_id)
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