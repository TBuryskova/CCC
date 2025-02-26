library(dplyr) # Optional, for data manipulation if needed
library(stringr)
library(scales)
library(readr)
library(lubridate)
library(stargazer)
library(tidyr)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(multiwayvcov)
library(stringr)
library(tidyverse)
library(purrr)
library(xtable)

data_basic <- read_rds("data_basic.rds")
chambers <- read.csv("../data/csv/chamber compositions.csv")
substitute <- read.csv("../data/csv/substitute chamber members.csv")


chambers <- chambers %>%
  mutate(start_date = dmy(start_date),
         end_date = dmy(end_date))

data_basic <- data_basic %>% 
  mutate(formation=case_when(formation=="First Chamber" ~ "1st",
                             formation=="Second Chamber" ~ "2nd",
                             formation=="Third Chamber" ~ "3rd",
                             formation=="Fourth Chamber" ~ "4th"))

decisions_with_judges <- data_basic %>%
  left_join(chambers, by = c("formation" = "chamber_id")) %>%
  filter(ymd(date_submission) >= start_date & ymd(date_submission) <= end_date) %>%
  group_by(chamber_id, date_submission) %>%
  summarise(judges = paste(sort(unique(judge_id)), collapse = "")) %>%
  ungroup()

final_decisions <- data_basic %>%
  left_join(decisions_with_judges, by = c("chamber_id", "date_submission")) 

check <- final_decisions  %>% filter(chamber_id==judges)
