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

data_cleanJE <- read_rds("data_cleanJE.rds")
initial <- read_rds("initial.rds")


# Regressing the chamber fixed effect on the characteristics of the chamber
data_cleanJE<- data_cleanJE %>%
  group_by(judge) %>%
  summarize(
    gender=first(gender), uni=first(uni), background=first(background), var_yob=max(var_yob),average_yob=max(average_yob),
       FE_lp=mean(FE_lp),FE_o=mean(FE_o),FE_cpy=mean(FE_cpy),
    scholar=first(scholar) ,
    outcome=mean(outcome), cited_per_year=mean(cited_per_year) 
  ) %>%

  left_join(initial, by=c("judge"="judge_id"),suffix = c("","C"))
  

length_proceeding <- lm(FE_lp~ average_yobC+ var_yobC+ scholarC+genderC, data_cleanJE)
summary(length_proceeding)

outcome <- lm(FE_o ~  average_yobC+ var_yobC +scholarC+genderC,  data_cleanJE)
summary(outcome)


cited_per_year <- lm(FE_cpy~  average_yobC+ var_yobC +scholarC+genderC,  data_cleanJE)
summary(cited_per_year)


stargazer(length_proceeding,outcome ,cited_per_year,
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))
