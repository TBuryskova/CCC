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
       FE_lp=mean(FE_lp),FE_o=mean(FE_o),FE_c=mean(FE_c),
    scholar=first(scholar) ,
    outcome=mean(outcome), cited=mean(cited) 
  ) %>%

  left_join(initial, by=c("judge"="judge_id"),suffix = c("","C"))
  

length_proceeding <- lm(FE_lp~ average_yobC+ var_yobC+ scholarC+genderC , data_cleanJE)
summary(length_proceeding)

outcome <- lm(FE_o ~  average_yobC+ var_yobC +scholarC+genderC ,  data_cleanJE)
summary(outcome)


cited <- lm(FE_c~  average_yobC+ var_yobC +scholarC+genderC ,  data_cleanJE)
summary(cited)


stargazer(length_proceeding,outcome ,cited,
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))
