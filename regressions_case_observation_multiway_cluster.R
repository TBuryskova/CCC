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
data_clean_100 <- data_clean %>%
  group_by(chamber_id) %>%            
  filter(n() > 100) %>%                
  ungroup()  
n_chambers <- data_clean_100 %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding_100 <- lm(length_proceeding~chamber_id+year_decision+judge_rapporteur_id+
                                         judge1+judge2+judge3
                                       , data_clean_100)
reduced_model_length_proceeding_100 <- lm(length_proceeding~year_decision+judge_rapporteur_id+
                                            judge1+judge2+judge3, data_clean_100)

anova(reduced_model_length_proceeding_100, full_model_length_proceeding_100)

full_model_outcome_100 <- lm(outcome~chamber_id+year_decision+judge_rapporteur_id+
                               judge1+judge2+judge3
                             , data=data_clean_100)
reduced_model_outcome_100 <- lm(outcome~year_decision+judge_rapporteur_id+
                                  judge1+judge2+judge3, data_clean_100)

anova(reduced_model_outcome_100, full_model_outcome_100)

full_model_cited_per_year_100 <- lm(cited_per_year~chamber_id+year_decision+judge_rapporteur_id+
                                        judge1+judge2+judge3
                                      , data_clean_100)
reduced_model_cited_per_year_100 <- lm(cited_per_year~year_decision+judge_rapporteur_id+
                                           judge1+judge2+judge3
                                         , data_clean_100)

anova(reduced_model_cited_per_year_100, full_model_cited_per_year_100)

# full_model_length_proceeding_100 <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                    judge1+judge2+judge3+n_applicants+controversial+
#                    n_disputed_act +
#                    n_concerned_act + n_concerned_cact + n_topics + 
#                   n_topicsc, data_clean_100)
# reduced_model_length_proceeding_100 <- lm(length_proceeding~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                       judge1+judge2+judge3+n_applicants+controversial+
#                       n_disputed_act +
#                       n_concerned_act + n_concerned_cact +  n_topics + 
#                        n_topicsc, data_clean_100)
# 
# anova(reduced_model_length_proceeding_100, full_model_length_proceeding_100)
# 
# 
# full_model_outcome_100 <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                    judge1+judge2+judge3+n_applicants+controversial+
#                    n_disputed_act +
#                    n_concerned_act + n_concerned_cact  + n_topics +
#                  n_topicsc, data_clean_100)
# reduced_model_outcome_100 <- lm(outcome~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                       judge1+judge2+judge3+n_applicants+controversial+
#                       n_disputed_act +
#                       n_concerned_act + n_concerned_cact + n_topics +
#                        n_topicsc, data_clean_100)
# 
#  anova(reduced_model_outcome_100, full_model_outcome_100)
# 
# full_model_cited_per_year_100 <- lm(cited_per_year~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                            judge1+judge2+judge3+n_applicants+controversial+
#                            n_disputed_act +
#                            n_concerned_act + n_concerned_cact + n_topics + 
#                             n_topicsc, data_clean_100)
# reduced_model_cited_per_year_100 <- lm(cited_per_year~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                               judge1+judge2+judge3+n_applicants+
#                               n_disputed_act +controversial+
#                               n_concerned_act + n_concerned_cact +  n_topics + 
#                               n_topicsc, data_clean_100)
# 
# anova(reduced_model_cited_per_year_100, full_model_cited_per_year_100)


# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding_100)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

data_cleanCE_100 <- left_join(data_clean_100,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome_100)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

data_cleanCE_100 <- left_join(data_cleanCE_100,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_cited_per_year_100)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_cpy <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_cpy = chamber_effects, row.names= NULL
)

data_cleanCE_100 <- left_join(data_cleanCE_100,chamber_effects_cpy, by="chamber_id")



# Regressing the chamber fixed effect on the characteristics of the chamber


length_proceeding_100 <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_100)

outcome_100 <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_100)

cited_per_year_100 <- lm(FE_cpy~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_100)

length_proceeding_C_100 <- lm(FE_lp~     n_applicants+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + n_topics + 
                                + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_100)
summary(length_proceeding_C_100)

outcome_C_100 <- lm(FE_o~ n_applicants+
                      n_disputed_act +controversial+
                      n_concerned_act + n_concerned_cact + n_topics + 
                      + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_100)

cited_per_year_C_100 <- lm(FE_cpy~ n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_100)


data_clean_50 <- data_clean %>%
  group_by(chamber_id) %>%            
  filter(n() > 50) %>%                
  ungroup()  
n_chambers <- data_clean_50 %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding_50 <- lm(length_proceeding~chamber_id+judge_rapporteur_id+
                                        judge1+judge2+judge3
                                      , data_clean_50)
reduced_model_length_proceeding_50 <- lm(length_proceeding~judge_rapporteur_id+
                                           judge1+judge2+judge3, data_clean_50)

anova(reduced_model_length_proceeding_50, full_model_length_proceeding_50)

full_model_outcome_50 <- lm(outcome~chamber_id+judge_rapporteur_id+
                              judge1+judge2+judge3
                            , data=data_clean_50)
reduced_model_outcome_50 <- lm(outcome~judge_rapporteur_id+
                                 judge1+judge2+judge3, data_clean_50)

anova(reduced_model_outcome_50, full_model_outcome_50)

full_model_cited_per_year_50 <- lm(cited_per_year~chamber_id+judge_rapporteur_id+
                                       judge1+judge2+judge3
                                     , data_clean_50)
reduced_model_cited_per_year_50 <- lm(cited_per_year~judge_rapporteur_id+
                                          judge1+judge2+judge3
                                        , data_clean_50)

anova(reduced_model_cited_per_year_50, full_model_cited_per_year_50)
# 
# full_model_length_proceeding_50 <- lm(length_proceeding~chamber_id+type_proceedings+importance+judge_rapporteur_id+
#                                          judge1+judge2+judge3+n_applicants+controversial+
#                                          n_disputed_act +
#                                          n_concerned_act + n_concerned_cact + n_topics + 
#                                          n_topicsc, data_clean_50)
# reduced_model_length_proceeding_50 <- lm(length_proceeding~type_proceedings+importance+judge_rapporteur_id+
#                                             judge1+judge2+judge3+n_applicants+controversial+
#                                             n_disputed_act +
#                                             n_concerned_act + n_concerned_cact +  n_topics + 
#                                             n_topicsc, data_clean_50)
# 
# anova(reduced_model_length_proceeding_50, full_model_length_proceeding_50)
# 
# 
# full_model_outcome_50 <- lm(outcome~chamber_id+type_proceedings+importance+judge_rapporteur_id+
#                                judge1+judge2+judge3+n_applicants+controversial+
#                                n_disputed_act +
#                                n_concerned_act + n_concerned_cact  + n_topics +
#                                n_topicsc, data_clean_50)
# reduced_model_outcome_50 <- lm(outcome~type_proceedings+importance+judge_rapporteur_id+
#                                   judge1+judge2+judge3+n_applicants+controversial+
#                                   n_disputed_act +
#                                   n_concerned_act + n_concerned_cact + n_topics +
#                                   n_topicsc, data_clean_50)
# 
# anova(reduced_model_outcome_50, full_model_outcome_50)
# 
# full_model_cited_per_year_50 <- lm(cited_per_year~chamber_id+type_proceedings+importance+judge_rapporteur_id+
#                                         judge1+judge2+judge3+n_applicants+controversial+
#                                         n_disputed_act +
#                                         n_concerned_act + n_concerned_cact + n_topics + 
#                                         n_topicsc, data_clean_50)
# reduced_model_cited_per_year_50 <- lm(cited_per_year~type_proceedings+importance+judge_rapporteur_id+
#                                            judge1+judge2+judge3+n_applicants+
#                                            n_disputed_act +controversial+
#                                            n_concerned_act + n_concerned_cact +  n_topics + 
#                                            n_topicsc, data_clean_50)
# 
# anova(reduced_model_cited_per_year_50, full_model_cited_per_year_50)


# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding_50)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

data_cleanCE_50 <- left_join(data_clean_50,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome_50)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

data_cleanCE_50 <- left_join(data_cleanCE_50,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_cited_per_year_50)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_cpy <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_cpy = chamber_effects, row.names= NULL
)

data_cleanCE_50 <- left_join(data_cleanCE_50,chamber_effects_cpy, by="chamber_id")




length_proceeding_50 <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_50)

outcome_50 <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_50)

cited_per_year_50 <- lm(FE_cpy~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_50)

length_proceeding_C_50 <- lm(FE_lp~     n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_50)

outcome_C_50 <- lm(FE_o~ n_applicants+
                     n_disputed_act +controversial+
                     n_concerned_act + n_concerned_cact + n_topics + 
                     + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_50)

cited_per_year_C_50 <- lm(FE_cpy~ n_applicants+
                              n_disputed_act +controversial+
                              n_concerned_act + n_concerned_cact + n_topics + 
                              + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_50)



data_clean_20 <- data_clean %>%
  group_by(chamber_id) %>%            
  filter(n() > 20) %>%                
  ungroup()  
n_chambers <- data_clean_20 %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding_20 <- lm(length_proceeding~chamber_id+judge_rapporteur_id+
                                        judge1+judge2+judge3
                                      , data_clean_20)
reduced_model_length_proceeding_20 <- lm(length_proceeding~judge_rapporteur_id+
                                           judge1+judge2+judge3, data_clean_20)

anova(reduced_model_length_proceeding_20, full_model_length_proceeding_20)

full_model_outcome_20 <- lm(outcome~chamber_id+judge_rapporteur_id+
                              judge1+judge2+judge3
                            , data=data_clean_20)
reduced_model_outcome_20 <- lm(outcome~judge_rapporteur_id+
                                 judge1+judge2+judge3, data_clean_20)

anova(reduced_model_outcome_20, full_model_outcome_20)

full_model_cited_per_year_20 <- lm(cited_per_year~chamber_id+judge_rapporteur_id+
                                       judge1+judge2+judge3
                                     , data_clean_20)
reduced_model_cited_per_year_20 <- lm(cited_per_year~judge_rapporteur_id+
                                          judge1+judge2+judge3
                                        , data_clean_20)

anova(reduced_model_cited_per_year_20, full_model_cited_per_year_20)
# 
# full_model_length_proceeding_20 <- lm(length_proceeding~chamber_id+type_proceedings+importance+judge_rapporteur_id+
#                                         judge1+judge2+judge3+n_applicants+controversial+
#                                         n_disputed_act +
#                                         n_concerned_act + n_concerned_cact + n_topics + 
#                                         n_topicsc, data_clean_20)
# reduced_model_length_proceeding_20 <- lm(length_proceeding~type_proceedings+importance+judge_rapporteur_id+
#                                            judge1+judge2+judge3+n_applicants+controversial+
#                                            n_disputed_act +
#                                            n_concerned_act + n_concerned_cact +  n_topics + 
#                                            n_topicsc, data_clean_20)
# 
# anova(reduced_model_length_proceeding_20, full_model_length_proceeding_20)
# 
# 
# full_model_outcome_20 <- lm(outcome~chamber_id+type_proceedings+importance+judge_rapporteur_id+
#                               judge1+judge2+judge3+n_applicants+controversial+
#                               n_disputed_act +
#                               n_concerned_act + n_concerned_cact  + n_topics +
#                               n_topicsc, data_clean_20)
# reduced_model_outcome_20 <- lm(outcome~type_proceedings+importance+judge_rapporteur_id+
#                                  judge1+judge2+judge3+n_applicants+controversial+
#                                  n_disputed_act +
#                                  n_concerned_act + n_concerned_cact + n_topics +
#                                  n_topicsc, data_clean_20)
# 
# anova(reduced_model_outcome_20, full_model_outcome_20)
# 
# full_model_cited_per_year_20 <- lm(cited_per_year~chamber_id+type_proceedings+importance+judge_rapporteur_id+
#                                        judge1+judge2+judge3+n_applicants+controversial+
#                                        n_disputed_act +
#                                        n_concerned_act + n_concerned_cact + n_topics + 
#                                        n_topicsc, data_clean_20)
# reduced_model_cited_per_year_20 <- lm(cited_per_year~type_proceedings+importance+judge_rapporteur_id+
#                                           judge1+judge2+judge3+n_applicants+
#                                           n_disputed_act +controversial+
#                                           n_concerned_act + n_concerned_cact +  n_topics + 
#                                           n_topicsc, data_clean_20)
# 
# anova(reduced_model_cited_per_year_20, full_model_cited_per_year_20)
# 

# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding_20)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

data_cleanCE_20 <- left_join(data_clean_20,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome_20)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

data_cleanCE_20 <- left_join(data_cleanCE_20,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_cited_per_year_20)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_cpy <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_cpy = chamber_effects, row.names= NULL
)

data_cleanCE_20 <- left_join(data_cleanCE_20,chamber_effects_cpy, by="chamber_id")



# Regressing the chamber fixed effect on the characteristics of the chamber


length_proceeding_20 <- lm(FE_lp~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_20)

outcome_20 <- lm(FE_o ~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_20)

cited_per_year_20 <- lm(FE_cpy~  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_20)

length_proceeding_C_20 <- lm(FE_lp~     n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_20)

outcome_C_20 <- lm(FE_o~ n_applicants+
                     n_disputed_act +controversial+
                     n_concerned_act + n_concerned_cact + n_topics + 
                     + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_20)

cited_per_year_C_20 <- lm(FE_cpy~ n_applicants+
                              n_disputed_act +controversial+
                              n_concerned_act + n_concerned_cact + n_topics + 
                              + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender,  data_cleanCE_20)

clustered_se_100 <- cluster.vcov(length_proceeding_100, cbind(data_cleanCE_100$judge1, data_cleanCE_100$judge2, data_cleanCE_100$judge3))
clustered_se_C_100 <- cluster.vcov(length_proceeding_C_100, cbind(data_cleanCE_100$judge1, data_cleanCE_100$judge2, data_cleanCE_100$judge3))
clustered_se_50 <- cluster.vcov(length_proceeding_50, cbind(data_cleanCE_50$judge1, data_cleanCE_50$judge2, data_cleanCE_50$judge3))
clustered_se_C_50 <- cluster.vcov(length_proceeding_C_50, cbind(data_cleanCE_50$judge1, data_cleanCE_50$judge2, data_cleanCE_50$judge3))
clustered_se_20 <- cluster.vcov(length_proceeding_20, cbind(data_cleanCE_20$judge1, data_cleanCE_20$judge2, data_cleanCE_20$judge3))
clustered_se_C_20 <- cluster.vcov(length_proceeding_C_20, cbind(data_cleanCE_20$judge1, data_cleanCE_20$judge2, data_cleanCE_20$judge3))

# Repeat for the other models
clustered_se_outcome_100 <- cluster.vcov(outcome_100, cbind(data_cleanCE_100$judge1, data_cleanCE_100$judge2, data_cleanCE_100$judge3))
clustered_se_outcome_C_100 <- cluster.vcov(outcome_C_100, cbind(data_cleanCE_100$judge1, data_cleanCE_100$judge2, data_cleanCE_100$judge3))
clustered_se_outcome_50 <- cluster.vcov(outcome_50, cbind(data_cleanCE_50$judge1, data_cleanCE_50$judge2, data_cleanCE_50$judge3))
clustered_se_outcome_C_50 <- cluster.vcov(outcome_C_50, cbind(data_cleanCE_50$judge1, data_cleanCE_50$judge2, data_cleanCE_50$judge3))
clustered_se_outcome_20 <- cluster.vcov(outcome_20, cbind(data_cleanCE_20$judge1, data_cleanCE_20$judge2, data_cleanCE_20$judge3))
clustered_se_outcome_C_20 <- cluster.vcov(outcome_C_20, cbind(data_cleanCE_20$judge1, data_cleanCE_20$judge2, data_cleanCE_20$judge3))

clustered_se_cpy_100 <- cluster.vcov(cited_per_year_100, cbind(data_cleanCE_100$judge1, data_cleanCE_100$judge2, data_cleanCE_100$judge3))
clustered_se_cpy_C_100 <- cluster.vcov(cited_per_year_C_100, cbind(data_cleanCE_100$judge1, data_cleanCE_100$judge2, data_cleanCE_100$judge3))
clustered_se_cpy_50 <- cluster.vcov(cited_per_year_50, cbind(data_cleanCE_50$judge1, data_cleanCE_50$judge2, data_cleanCE_50$judge3))
clustered_se_cpy_C_50 <- cluster.vcov(cited_per_year_C_50, cbind(data_cleanCE_50$judge1, data_cleanCE_50$judge2, data_cleanCE_50$judge3))
clustered_se_cpy_20 <- cluster.vcov(cited_per_year_20, cbind(data_cleanCE_20$judge1, data_cleanCE_20$judge2, data_cleanCE_20$judge3))
clustered_se_cpy_C_20 <- cluster.vcov(cited_per_year_C_20, cbind(data_cleanCE_20$judge1, data_cleanCE_20$judge2, data_cleanCE_20$judge3))

stargazer(length_proceeding_100, length_proceeding_C_100, length_proceeding_50, length_proceeding_C_50, length_proceeding_20, length_proceeding_C_20,
          se = list(sqrt(diag(clustered_se_100)), sqrt(diag(clustered_se_C_100)), sqrt(diag(clustered_se_50)), sqrt(diag(clustered_se_C_50)), 
                    sqrt(diag(clustered_se_20)), sqrt(diag(clustered_se_C_20))),
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))

# Outcome Models
stargazer(outcome_100, outcome_C_100, outcome_50, outcome_C_50, outcome_20, outcome_C_20,
          se = list(sqrt(diag(clustered_se_outcome_100)), sqrt(diag(clustered_se_outcome_C_100)), sqrt(diag(clustered_se_outcome_50)), 
                    sqrt(diag(clustered_se_outcome_C_50)), sqrt(diag(clustered_se_outcome_20)), sqrt(diag(clustered_se_outcome_C_20))),
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))

# Has Popular Name Models
stargazer(cited_per_year_100, cited_per_year_C_100, cited_per_year_50, cited_per_year_C_50, cited_per_year_20, cited_per_year_C_20,
          se = list(sqrt(diag(clustered_se_cpy_100)), sqrt(diag(clustered_se_cpy_C_100)), sqrt(diag(clustered_se_cpy_50)), 
                    sqrt(diag(clustered_se_cpy_C_50)), sqrt(diag(clustered_se_cpy_20)), sqrt(diag(clustered_se_cpy_C_20))),
          omit = c("^year", "^judge", "^n", "controversial", "Constant"))