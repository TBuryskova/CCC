library(dplyr) # Optional, for data manipulation if needed
library(stringr)
library(scales)
library(readr)
library(lubridate)
library(stargazer)
library(tidyr)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(multiwayvcov)
library(stringr)
library(tidyverse)
library(purrr)
library(xtable)

data_cleanCE <- read_rds("data_cleanCE.rds")

########### Regressions fixed effects only ##################  

# Regressing the chamber fixed effect on the characteristics of the chamber
data_cleanCE<- data_cleanCE %>% group_by(chamber_id_alt) %>% filter(n()>100) %>% ungroup()

length_proceeding <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender + year_submission, data_cleanCE)

outcome <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender + year_submission,  data_cleanCE)

cited <- lm(FE_c~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender + year_submission+grounds,  data_cleanCE)

length_proceeding_C <- lm(FE_lp~     n_applicants+
                            n_disputed_act +controversial+
                            n_concerned_act + n_concerned_cact + n_topics + 
                            + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender + year_submission, data_cleanCE)
summary(length_proceeding_C)

outcome_C <- lm(FE_o~ n_applicants+
                  n_disputed_act +controversial+
                  n_concerned_act + n_concerned_cact + n_topics + 
                  + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender + year_submission, data_cleanCE)

cited_C <- lm(FE_c~ n_applicants+
                         n_disputed_act +controversial+
                         n_concerned_act + n_concerned_cact + n_topics + 
                         + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender + year_submission+grounds,  data_cleanCE)


clustered_se <- cluster.vcov(length_proceeding, data_cleanCE$chamber_id_alt)
clustered_se_C <- cluster.vcov(length_proceeding_C, data_cleanCE$chamber_id_alt)

# Repeat for the other models
clustered_se_outcome <- cluster.vcov(outcome, data_cleanCE$chamber_id_alt)
clustered_se_outcome_C <- cluster.vcov(outcome_C, data_cleanCE$chamber_id_alt)

clustered_se_c <- cluster.vcov(cited, data_cleanCE$chamber_id_alt)
clustered_se_c_C <- cluster.vcov(cited_C, data_cleanCE$chamber_id_alt)

stargazer(length_proceeding, length_proceeding_C,outcome, outcome_C,cited, cited_C,
          se = list(sqrt(diag(clustered_se)), sqrt(diag(clustered_se_C)), sqrt(diag(clustered_se_outcome)), sqrt(diag(clustered_se_outcome_C)), sqrt(diag(clustered_se_c)), sqrt(diag(clustered_se_c_C))),
          omit = c("^year", "^judge", "^n", "controversial","^grounds", "Constant"))
