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

full_model_outcome <- lm(outcome~chamber_id+year_decision
                             , data=data_clean)
reduced_model_outcome <- lm(outcome~year_decision, data_clean)

anova(reduced_model_outcome, full_model_outcome)

full_model_cited_per_year <- lm(cited_per_year~chamber_id
                                      , data_clean)
reduced_model_cited_per_year <- lm(cited_per_year~year_decision
                                         , data_clean)

anova(reduced_model_cited_per_year, full_model_cited_per_year)

# full_model_length_proceeding <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                    judge1+judge2+judge3+n_applicants+controversial+
#                    n_disputed_act +
#                    n_concerned_act + n_concerned_cact + n_topics + 
#                   n_topicsc, data_clean)
# reduced_model_length_proceeding <- lm(length_proceeding~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                       judge1+judge2+judge3+n_applicants+controversial+
#                       n_disputed_act +
#                       n_concerned_act + n_concerned_cact +  n_topics + 
#                        n_topicsc, data_clean)
# 
# anova(reduced_model_length_proceeding, full_model_length_proceeding)
# 
# 
# full_model_outcome <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                    judge1+judge2+judge3+n_applicants+controversial+
#                    n_disputed_act +
#                    n_concerned_act + n_concerned_cact  + n_topics +
#                  n_topicsc, data_clean)
# reduced_model_outcome <- lm(outcome~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                       judge1+judge2+judge3+n_applicants+controversial+
#                       n_disputed_act +
#                       n_concerned_act + n_concerned_cact + n_topics +
#                        n_topicsc, data_clean)
# 
#  anova(reduced_model_outcome, full_model_outcome)
# 
# full_model_cited_per_year <- lm(cited_per_year~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                            judge1+judge2+judge3+n_applicants+controversial+
#                            n_disputed_act +
#                            n_concerned_act + n_concerned_cact + n_topics + 
#                             n_topicsc, data_clean)
# reduced_model_cited_per_year <- lm(cited_per_year~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                               judge1+judge2+judge3+n_applicants+
#                               n_disputed_act +controversial+
#                               n_concerned_act + n_concerned_cact +  n_topics + 
#                               n_topicsc, data_clean)
# 
# anova(reduced_model_cited_per_year, full_model_cited_per_year)


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
