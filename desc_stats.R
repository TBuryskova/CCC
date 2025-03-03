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



selected_vars <- c("n_applicants", "n_citations", "n_disputed_act",
                   "n_concerned_act","n_concerned_cact", "length_proceeding", "controversial", "meritory", 
                   "has_popular_name", "outcome")  # Replace with your variables

desc_stats_num <- data_clean%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.numeric, funs(mean=mean(., na.rm=TRUE)))

desc_stats_log <- data_clean%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.logical, funs(mean)) 

desc_stats <- desc_stats_num %>% cbind(desc_stats_log) %>% t()

desc_stats_num <- data_clean%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.numeric, funs(max=max(., na.rm=TRUE)
  )) %>% t()