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
data_cleanCE<- data_cleanCE %>% group_by(chamber_id_alt) %>%
  group_by(chamber_id_alt) %>%
  summarize(
    gender=first(gender), uni=first(uni), background=first(background), var_yob=max(var_yob),average_yob=max(average_yob),
    length_proceeding=mean(length_proceeding),n_applicants=mean(n_applicants), n_disputed_act=mean(n_disputed_act),
    controversial=mean(controversial),n_concerned_act=mean(n_concerned_act),n_concerned_cact=mean(n_concerned_cact),n_topics=mean(n_topics),
    FE_lp=mean(FE_lp),FE_o=mean(FE_o),FE_c=mean(FE_c),

    outcome=mean(outcome), cited=mean(cited) ,
    same_background=mean(same_background),
    all_different_background=mean(all_different_background),
    same_uni=mean(same_uni), scholar=mean(scholar)
  )


length_proceeding <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE)
summary(length_proceeding)

outcome <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE)
summary(outcome)


cited <- lm(FE_c~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE)
summary(cited)

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

cited_C <- lm(FE_c~ n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact +  

                               + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE)
summary(cited_C)



stargazer(length_proceeding, length_proceeding_C,outcome, outcome_C,cited, cited_C,
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))
