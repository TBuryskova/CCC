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

length_proceeding <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender+judge1+judge2+judge3, data_cleanCE)

outcome <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender+judge1+judge2+judge3,  data_cleanCE)

cited_per_year <- lm(FE_cpy~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender+judge1+judge2+judge3,  data_cleanCE)

length_proceeding_C <- lm(FE_lp~     n_applicants+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + n_topics + 
                                + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender+judge1+judge2+judge3, data_cleanCE)
summary(length_proceeding_C)

outcome_C <- lm(FE_o~ n_applicants+
                      n_disputed_act +controversial+
                      n_concerned_act + n_concerned_cact + n_topics + 
                      + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender+judge1+judge2+judge3, data_cleanCE)

cited_per_year_C <- lm(FE_cpy~ n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender+judge1+judge2+judge3,  data_cleanCE)


clustered_se <- cluster.vcov(length_proceeding, cbind(data_cleanCE$judge1, data_cleanCE$judge2, data_cleanCE$judge3))
clustered_se_C <- cluster.vcov(length_proceeding_C, cbind(data_cleanCE$judge1, data_cleanCE$judge2, data_cleanCE$judge3))

# Repeat for the other models
clustered_se_outcome <- cluster.vcov(outcome, cbind(data_cleanCE$judge1, data_cleanCE$judge2, data_cleanCE$judge3))
clustered_se_outcome_C <- cluster.vcov(outcome_C, cbind(data_cleanCE$judge1, data_cleanCE$judge2, data_cleanCE$judge3))

clustered_se_cpy <- cluster.vcov(cited_per_year, cbind(data_cleanCE$judge1, data_cleanCE$judge2, data_cleanCE$judge3))
clustered_se_cpy_C <- cluster.vcov(cited_per_year_C, cbind(data_cleanCE$judge1, data_cleanCE$judge2, data_cleanCE$judge3))

stargazer(length_proceeding, length_proceeding_C,
          se = list(sqrt(diag(clustered_se)), sqrt(diag(clustered_se_C))),
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))

stargazer(outcome, outcome_C,
          se = list(sqrt(diag(clustered_se_outcome)), sqrt(diag(clustered_se_outcome_C))),
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))

stargazer(cited_per_year, cited_per_year_C,
          se = list(sqrt(diag(clustered_se_cpy)), sqrt(diag(clustered_se_cpy_C))),
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))
