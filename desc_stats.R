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



selected_vars <- c("n_applicants", "n_citations", "n_disputed_act",
                   "n_concerned_act","n_concerned_cact", "length_proceeding", "controversial", "meritory", 
                   "has_popular_name", "outcome")  # Replace with your variables

desc_stats_num <- sample1623%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.numeric, funs(sd=sd(., na.rm=TRUE)))

desc_stats_log <- sample1623%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.logical, funs(mean)) 

desc_stats <- desc_stats_num %>% cbind(desc_stats_log) %>% t()

desc_stats_num <- sample1623%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.numeric, funs(min=min(., na.rm=TRUE)
  )) %>% t()