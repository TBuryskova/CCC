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

data_clean <- read_rds("data_clean.rds")

########### Regressions fixed effects only ##################  

n_chambers <- data_clean %>% summarise(n_distinct(chamber_id_alt))

full_model_length_proceeding <- lm(length_proceeding~chamber_id_alt+judge
                                   , data_clean)
reduced_model_length_proceeding <- lm(length_proceeding~judge, data_clean)

anova(reduced_model_length_proceeding, full_model_length_proceeding)

full_model_length_proceedingC <- lm(length_proceeding~chamber_id_alt+judge+year_decision+type_proceedings+importance+n_applicants+controversial+
                   n_disputed_act +
                   n_concerned_act + n_concerned_cact  +
                 n_topicsc + year_submission, data_clean)
reduced_model_length_proceedingC <- lm(length_proceeding~year_decision+judge+type_proceedings+importance+n_applicants+controversial+
                      n_disputed_act +
                      n_concerned_act + n_concerned_cact +
                       n_topicsc + year_submission, data_clean)
anova(reduced_model_length_proceedingC, full_model_length_proceedingC)



full_model_outcome <- lm(outcome~chamber_id_alt+judge
                         , data=data_clean)
reduced_model_outcome <- lm(outcome~judge, data_clean)

anova(reduced_model_outcome, full_model_outcome)

full_model_outcomeC <- lm(outcome~chamber_id_alt+judge+type_proceedings+importance+n_applicants+controversial+
                                      n_disputed_act +
                                      n_concerned_act + n_concerned_cact  +
                                      n_topicsc + year_submission, data_clean)
reduced_model_outcomeC <- lm(outcome~type_proceedings+judge+importance+n_applicants+controversial+
                                         n_disputed_act +
                                         n_concerned_act + n_concerned_cact +
                                         n_topicsc + year_submission, data_clean)
anova(reduced_model_outcomeC, full_model_outcomeC)




full_model_cited <- lm(cited~chamber_id_alt+judge
                                , data_clean)
reduced_model_cited <- lm(cited~judge
                                   , data_clean)

anova(reduced_model_cited, full_model_cited)

full_model_citedC <- lm(cited~chamber_id_alt+judge+year_decision+type_proceedings+importance+n_applicants+controversial+
                                      n_disputed_act +
                                      n_concerned_act + n_concerned_cact  +
                                      n_topicsc + year_submission, data_clean)
reduced_model_citedC <- lm(cited~year_decision+judge+type_proceedings+importance+n_applicants+controversial+
                                         n_disputed_act +
                                         n_concerned_act + n_concerned_cact +
                                         n_topicsc + year_submission, data_clean)
anova(reduced_model_citedC, full_model_citedC)




# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding)
chamber_effects <- coefficients[grepl("chamber_id_alt", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id_alt = str_replace(names(chamber_effects) , "chamber_id_alt",""),
  FE_lp = chamber_effects, row.names= NULL
)

data_cleanCE <- left_join(data_clean,chamber_effects_lp, by="chamber_id_alt")

coefficients <- coef(full_model_outcome)
chamber_effects <- coefficients[grepl("chamber_id_alt", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id_alt = str_replace(names(chamber_effects) , "chamber_id_alt",""),
  FE_o = chamber_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,chamber_effects_o, by="chamber_id_alt")


coefficients <- coef(full_model_cited)
chamber_effects <- coefficients[grepl("chamber_id_alt", names(coefficients))]
chamber_effects_c <- data.frame(
  chamber_id_alt = str_replace(names(chamber_effects) , "chamber_id_alt",""),
  FE_c = chamber_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,chamber_effects_c, by="chamber_id_alt")

saveRDS(data_cleanCE, file="data_cleanCE.rds")
