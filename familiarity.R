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

sample1623 <- read_rds("sample1623.rds")

judge_input = "Kateřina Šimáčková"
familiarity <-  sample1623 %>%  filter(judge_name == judge_input |
                           judge_name2 == judge_input |
                           judge_name3 == judge_input) %>% 
    select(date_submission, judge_name, judge_name2, judge_name3)


ggplot(familiarity) +
  geom_point(aes(y = ifelse(judge_name2 == judge_input, NA, judge_name2), x = ymd(date_submission)), na.rm = TRUE) +
  geom_point(aes(y = ifelse(judge_name3 == judge_input, NA, judge_name3), x = ymd(date_submission)), na.rm = TRUE) +
  scale_y_discrete(na.translate = FALSE)  