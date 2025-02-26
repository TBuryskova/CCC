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
  mutate(
    start_date = dmy(start_date),
         end_date = dmy(end_date)) %>% 
  mutate(end_date=case_when(is.na(end_date)~ dmy("31/12/2024"),
         TRUE ~ end_date) )%>%
  rename(chamber_number=chamber_id)

substitute <- substitute %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)) %>% 
  mutate(end_date=case_when(is.na(end_date)~ dmy("31/12/2024"),
                            TRUE ~ end_date) )%>%
  rename(chamber_number=chamber_id)

data_basic <- data_basic %>% 
  mutate(formation=case_when(formation=="First Chamber" ~ "1st",
                             formation=="Second Chamber" ~ "2nd",
                             formation=="Third Chamber" ~ "3rd",
                             formation=="Fourth Chamber" ~ "4th"))

decisions_with_judges <- data_basic %>%
  left_join(chambers, by = c("formation" = "chamber_number")) %>%
  filter(ymd(date_submission) >= start_date & ymd(date_submission) <= end_date) %>%
  group_by(doc_id, formation, date_submission) %>%
  summarise(asjudge1 = sort(unique(judge_id))[1],
            asjudge2 = sort(unique(judge_id))[2],
            asjudge3 = sort(unique(judge_id))[3]) %>%
  ungroup() 

decisions_with_substitutes <- data_basic %>%
  left_join(substitute, by = c("formation" = "chamber_number")) %>%
  filter(ymd(date_submission) >= start_date & ymd(date_submission) <= end_date) %>%
  group_by(doc_id, formation, date_submission) %>%
  summarise(subjudgeA = sort(unique(judge_id))[1],
            subjudgeB = sort(unique(judge_id))[2]) %>%
  ungroup()

data_basic <- data_basic %>%
  left_join(decisions_with_judges, by = c("doc_id")) %>%
  left_join(decisions_with_substitutes, by = c("doc_id")) %>%
  mutate(composition_ok =  if_all(c(judge1, judge2, judge3), ~ . %in% c(asjudge1, asjudge2, asjudge3, subjudgeA, subjudgeB))
)

data_clean <- data_basic   %>% filter(composition_ok)

saveRDS(data_basic, "data_basic.rds")
saveRDS(data_clean, "data_clean.rds")
