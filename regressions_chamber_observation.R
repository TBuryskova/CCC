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

data_cleanCE <- read_rds("data_cleanCE.rds")


# Regressing the chamber fixed effect on the characteristics of the chamber
data_cleanCE<- data_cleanCE %>% mutate(average_yob=(judge_yob+judge_yob+judge_yob3)/3,
                                               var_yob=(judge_yob^2+judge_yob2^2+judge_yob3^2)/3-(judge_yob+judge_yob2+judge_yob3)^2/9) %>%
  rowwise() %>%
  mutate(background = str_c(sort(c(str_sub(judge_profession, 1, 1), 
                                   str_sub(judge_profession2, 1, 1), 
                                   str_sub(judge_profession3, 1, 1))), 
                            collapse = ""),
         uni = str_c(sort(c(str_sub(as.character(judge_uni), 1, 1), 
                            str_sub(as.character(judge_uni2), 1, 1), 
                            str_sub(as.character(judge_uni3), 1, 1))), 
                     collapse = ""),
         gender = str_c(sort(c(str_sub(as.character(judge_gender), 1, 1), 
                               str_sub(as.character(judge_gender2), 1, 1), 
                               str_sub(as.character(judge_gender3), 1, 1)), decreasing = TRUE), 
                        collapse = "")) %>%
  mutate(gender=  factor(gender, levels = c("MMM", "MMF", "MFF")) ) %>%
  ungroup() %>%
  group_by(chamber_id) %>%
  summarize(
    gender=first(gender), uni=first(uni), background=first(background), var_yob=max(var_yob),average_yob=max(average_yob),
    length_proceeding=mean(length_proceeding),n_applicants=mean(n_applicants), n_disputed_act=mean(n_disputed_act),
    controversial=mean(controversial),n_concerned_act=mean(n_concerned_act),n_concerned_cact=mean(n_concerned_cact),n_topics=mean(n_topics),
    FE_lp=mean(FE_lp),FE_o=mean(FE_o),FE_cpy=mean(FE_cpy),

    outcome=mean(outcome), cited_per_year=mean(cited_per_year) ,
    same_background=mean(same_background),
    all_different_background=mean(all_different_background),
    same_uni=mean(same_uni), scholar=mean(scholar)
  )


length_proceeding <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE)
summary(length_proceeding)

outcome <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE)
summary(outcome)


cited_per_year <- lm(FE_cpy~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE)
summary(cited_per_year)

length_proceeding_C <- lm(FE_lp~    n_applicants+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + 
                                + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE)
summary(length_proceeding_C)

outcome_C <- lm(FE_o~ n_applicants+
                      n_disputed_act +controversial+
                      n_concerned_act + n_concerned_cact +  
                      + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE)
summary(outcome_C)

cited_per_year_C <- lm(FE_cpy~ n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact +  

                               + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE)
summary(cited_per_year_C)



stargazer(length_proceeding, length_proceeding_C,outcome, outcome_C,cited_per_year, cited_per_year_C,
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))
