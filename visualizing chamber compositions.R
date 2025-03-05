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

data_basic <- read_rds("data_basic.rds")
data_clean <- read_rds("data_clean.rds")



ggplot(data_clean ) +
  geom_point(aes(x=date_submission, y=asjudge1, color=formation)) +
  geom_point(aes(x=date_submission,y=asjudge2, color=formation)) +
  geom_point(aes(x=date_submission, y=asjudge3, color=formation)) +
  geom_point(aes(x=date_submission, y=subjudgeA, color=formation), alpha=0.1) +
  geom_point(aes(x=date_submission, y=subjudgeB, color=formation), alpha=0.1) +
ylab("judge") +
  labs(color="chamber")

ggplot(data_basic ) +
  geom_point(aes(x=date_submission, y=judge, color=formation))
  geom_point(aes(x=date_submission, y=subjudgeA, color=formation), alpha=0.1) +
  geom_point(aes(x=date_submission, y=subjudgeB, color=formation), alpha=0.1) +
  ylab("judge")  +
labs(color="chamber")

ggplot(data_clean ) +
  geom_point(aes(x=date_submission, y=judge, color=formation)) +
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
  