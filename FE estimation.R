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

n_chambers <- data_clean %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding <- lm(length_proceeding~chamber_id+year_decision
                                   , data_clean)
reduced_model_length_proceeding <- lm(length_proceeding~year_decision, data_clean)

anova(reduced_model_length_proceeding, full_model_length_proceeding)

full_model_length_proceedingC <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+n_applicants+controversial+
                   n_disputed_act +
                   n_concerned_act + n_concerned_cact  +
                 n_topicsc, data_clean)
reduced_model_length_proceedingC <- lm(length_proceeding~year_decision+type_proceedings+importance+n_applicants+controversial+
                      n_disputed_act +
                      n_concerned_act + n_concerned_cact +
                       n_topicsc, data_clean)
anova(reduced_model_length_proceedingC, full_model_length_proceedingC)



full_model_outcome <- lm(outcome~chamber_id+year_decision
                         , data=data_clean)
reduced_model_outcome <- lm(outcome~year_decision, data_clean)

anova(reduced_model_outcome, full_model_outcome)

full_model_outcomeC <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+n_applicants+controversial+
                                      n_disputed_act +
                                      n_concerned_act + n_concerned_cact  +
                                      n_topicsc, data_clean)
reduced_model_outcomeC <- lm(outcome~year_decision+type_proceedings+importance+n_applicants+controversial+
                                         n_disputed_act +
                                         n_concerned_act + n_concerned_cact +
                                         n_topicsc, data_clean)
anova(reduced_model_outcomeC, full_model_outcomeC)




full_model_cited_per_year <- lm(cited_per_year~chamber_id
                                , data_clean)
reduced_model_cited_per_year <- lm(cited_per_year~year_decision
                                   , data_clean)

anova(reduced_model_cited_per_year, full_model_cited_per_year)

full_model_cited_per_yearC <- lm(cited_per_year~chamber_id+year_decision+type_proceedings+importance+n_applicants+controversial+
                                      n_disputed_act +
                                      n_concerned_act + n_concerned_cact  +
                                      n_topicsc, data_clean)
reduced_model_cited_per_yearC <- lm(cited_per_year~year_decision+type_proceedings+importance+n_applicants+controversial+
                                         n_disputed_act +
                                         n_concerned_act + n_concerned_cact +
                                         n_topicsc, data_clean)
anova(reduced_model_cited_per_yearC, full_model_cited_per_yearC)




# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

data_cleanCE <- left_join(data_clean,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_cited_per_year)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_cpy <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_cpy = chamber_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,chamber_effects_cpy, by="chamber_id")

saveRDS(data_cleanCE, file="data_cleanCE.rds")
