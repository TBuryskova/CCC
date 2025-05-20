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

get_surname <- function(x) str_trim(word(x, -1)) 

data_basic <- read_rds("data_basic.rds") %>%
  mutate(judge_name = get_surname(judge_name)) %>% 
  filter(!is.na(judge_name))

data_clean <- read_rds("data_clean.rds") %>%
  mutate(judge_name = get_surname(judge_name)) %>% 
  filter(!is.na(judge_name))


ggplot(data_basic ) +
  geom_point(aes(x=date_submission, y=judge_name, color=formation)) +
  geom_point(aes(x=date_submission, y=subjudgeA, color=formation), alpha=0.1) +
  geom_point(aes(x=date_submission, y=subjudgeB, color=formation), alpha=0.1) +
  ylab("judge")  +
labs(color="chamber")

ggplot(data_clean ) +
  geom_point(aes(x=date_submission, y=judge_name, color=formation)) +
  geom_point(aes(x=date_submission, y=subjudgeA, color=formation), alpha=0.1) +
  geom_point(aes(x=date_submission, y=subjudgeB, color=formation), alpha=0.1) +
  ylab("judge")  +
  labs(color="chamber")


data_basic %>%
  mutate(month = floor_date(date_submission, "month")) %>%
  group_by(month) %>%
  summarise(avg_composition_ok = mean(composition_ok, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = avg_composition_ok)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent) +
  labs( y = "% cases correct composition") +
  theme_minimal()
  