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

data_clean <- read_rds("data_clean.rds")



########### chamber_id balance ########
balance <- data_clean%>% group_by(chamber_id) %>% filter(n()>100) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
   n_applicants = length(applicant),
   
  ) %>%
ungroup() %>%
  group_by(chamber_id) %>%
  summarize(
    mean_n = mean(n_applicants, na.rm = TRUE),
    se_n = sd(n_applicants, na.rm = TRUE) / sqrt(n())
  )
  
ggplot(balance, aes(x=chamber_id, y = mean_n)) +
  geom_bar(fill="skyblue", stat='summary', fun='mean') +
labs(
    y = "Number of Concerned Acts",
    x = "Chamber"
  ) + 
  geom_errorbar(aes(ymin = mean_n - se_n, ymax = mean_n + se_n), width = 0.2) +
  labs(
    y = "Number of Applicants",
    x = "Chamber"
  ) +
  theme_minimal()

dow <- data_clean  %>%
  group_by(chamber_id) %>% filter(n()>100) %>%
  mutate(dow=wday(ymd(date_submission))) %>% ungroup()

ggplot(dow, aes(x = factor(dow), fill = chamber_id)) +
  geom_bar(aes(y = after_stat(prop), group = chamber_id), position = "dodge") +
  scale_x_discrete(
    labels = c("7" = "Sa", "1" = "Su", "2" = "Mo", "3" = "Tu", 
               "4" = "We", "5" = "Th", "6" = "Fr")
  ) +
  labs(
    y = "Proportion",
    x = "Day of the Week"
  ) +
  facet_wrap(~ chamber_id, ncol = 4) +  # Force vertical layout
  theme_minimal()


balance <- data_clean


aov(n_applicants ~ chamber_id, balance) %>% summary()
aov(dow ~ chamber_id, dow) %>% summary()

balance <- data_clean%>% group_by(chamber_id) %>%
  summarise(n=n())


ggplot(balance, aes(n)) +
  geom_histogram( binwidth=5, fill = "skyblue", color = "black") +
  
  labs(
    x = "Number of Cases",
    y = "Number of Chambers"
  ) +
  theme( text=element_text(size=20),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.background = element_blank())

ggplot(balance, aes(chamber_id, n)) +
  geom_col( fill = "skyblue", color = "black") +
  
  labs(
    x = "Chambers",
    y = "Number of Cases"
  ) +
  theme( text=element_text(size=20),
         axis.text.x = element_blank(),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.background = element_blank())
