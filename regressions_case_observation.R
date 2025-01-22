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

sample1623 <- read_rds("sample1623.rds")

########### Regressions fixed effects only ##################  
sample1623_100 <- sample1623 %>%
  group_by(chamber_id) %>%            
  filter(n() > 100) %>%                
  ungroup()  
n_chambers <- sample1623_100 %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding_100 <- lm(length_proceeding~chamber_id+year_decision+judge_rapporteur_id+
                                         judge1+judge2+judge3
                                       , sample1623_100)
reduced_model_length_proceeding_100 <- lm(length_proceeding~year_decision+judge_rapporteur_id+
                                            judge1+judge2+judge3, sample1623_100)

anova(reduced_model_length_proceeding_100, full_model_length_proceeding_100)

full_model_outcome_100 <- lm(outcome~chamber_id+year_decision+judge_rapporteur_id+
                               judge1+judge2+judge3
                             , data=sample1623_100)
reduced_model_outcome_100 <- lm(outcome~year_decision+judge_rapporteur_id+
                                  judge1+judge2+judge3, sample1623_100)

anova(reduced_model_outcome_100, full_model_outcome_100)

full_model_has_popular_name_100 <- lm(has_popular_name~chamber_id+year_decision+judge_rapporteur_id+
                                        judge1+judge2+judge3
                                      , sample1623_100)
reduced_model_has_popular_name_100 <- lm(has_popular_name~year_decision+judge_rapporteur_id+
                                           judge1+judge2+judge3
                                         , sample1623_100)

anova(reduced_model_has_popular_name_100, full_model_has_popular_name_100)

# full_model_length_proceeding_100 <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                    judge1+judge2+judge3+n_applicants+controversial+
#                    n_disputed_act +
#                    n_concerned_act + n_concerned_cact + n_topics + 
#                   n_topicsc, sample1623_100)
# reduced_model_length_proceeding_100 <- lm(length_proceeding~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                       judge1+judge2+judge3+n_applicants+controversial+
#                       n_disputed_act +
#                       n_concerned_act + n_concerned_cact +  n_topics + 
#                        n_topicsc, sample1623_100)
# 
# anova(reduced_model_length_proceeding_100, full_model_length_proceeding_100)
# 
# 
# full_model_outcome_100 <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                    judge1+judge2+judge3+n_applicants+controversial+
#                    n_disputed_act +
#                    n_concerned_act + n_concerned_cact  + n_topics +
#                  n_topicsc, sample1623_100)
# reduced_model_outcome_100 <- lm(outcome~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                       judge1+judge2+judge3+n_applicants+controversial+
#                       n_disputed_act +
#                       n_concerned_act + n_concerned_cact + n_topics +
#                        n_topicsc, sample1623_100)
# 
#  anova(reduced_model_outcome_100, full_model_outcome_100)
# 
# full_model_has_popular_name_100 <- lm(has_popular_name~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                            judge1+judge2+judge3+n_applicants+controversial+
#                            n_disputed_act +
#                            n_concerned_act + n_concerned_cact + n_topics + 
#                             n_topicsc, sample1623_100)
# reduced_model_has_popular_name_100 <- lm(has_popular_name~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                               judge1+judge2+judge3+n_applicants+
#                               n_disputed_act +controversial+
#                               n_concerned_act + n_concerned_cact +  n_topics + 
#                               n_topicsc, sample1623_100)
# 
# anova(reduced_model_has_popular_name_100, full_model_has_popular_name_100)


# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding_100)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

sample1623CE_100 <- left_join(sample1623_100,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome_100)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

sample1623CE_100 <- left_join(sample1623CE_100,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_has_popular_name_100)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_hpn <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_hpn = chamber_effects, row.names= NULL
)

sample1623CE_100 <- left_join(sample1623CE_100,chamber_effects_hpn, by="chamber_id")



# Regressing the chamber fixed effect on the characteristics of the chamber
sample1623CE_100<- sample1623CE_100 %>% mutate(average_yob=(judge_yob+judge_yob+judge_yob3)/3,
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
  ungroup() 

length_proceeding_100 <- lm(FE_lp~ year_decision+average_yob+ var_yob +background+uni+gender, sample1623CE_100)
summary(length_proceeding_100)

outcome_100 <- lm(FE_o ~  year_decision+average_yob+ var_yob +background+uni+gender,  sample1623CE_100)
summary(outcome_100)

has_popular_name_100 <- lm(FE_hpn~  year_decision+average_yob+ var_yob +background+uni+gender,  sample1623CE_100)
summary(has_popular_name_100)

length_proceeding_C_100 <- lm(length_proceeding~     year_decision+n_applicants+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + n_topics + 
                                + average_yob+ var_yob +background+uni+gender, sample1623CE_100)
summary(length_proceeding_C_100)

outcome_C_100 <- lm(outcome~ year_decision+n_applicants+
                      n_disputed_act +controversial+
                      n_concerned_act + n_concerned_cact + n_topics + 
                      + average_yob+ var_yob +background+uni+gender, sample1623CE_100)
summary(outcome_C_100)

has_popular_name_C_100 <- lm(has_popular_name~ year_decision+n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +background+uni+gender,  sample1623CE_100)
summary(has_popular_name_C_100)

length_proceeding_CF_100 <- lm(length_proceeding~  year_decision+   n_applicants+judge_rapporteur_id+
                                 n_disputed_act +controversial+
                                 n_concerned_act + n_concerned_cact + n_topics + 
                                 + average_yob+ var_yob +background+uni+gender, sample1623CE_100)
summary(length_proceeding_CF_100)


outcome_CF_100 <- lm(outcome~ year_decision+n_applicants+judge_rapporteur_id+
                       n_disputed_act +controversial+
                       n_concerned_act + n_concerned_cact + n_topics + 
                       + average_yob+ var_yob +background+uni+gender,  sample1623CE_100)
summary(outcome_CF_100)

has_popular_name_CF_100 <- lm(has_popular_name~ year_decision+n_applicants+judge_rapporteur_id+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + n_topics + 
                                + average_yob+ var_yob +background+uni+gender, sample1623CE_100)
summary(has_popular_name_CF_100)


sample1623_50 <- sample1623 %>%
  group_by(chamber_id) %>%            
  filter(n() > 50) %>%                
  ungroup()  
n_chambers <- sample1623_50 %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding_50 <- lm(length_proceeding~chamber_id+year_decision+judge_rapporteur_id+
                                        judge1+judge2+judge3
                                      , sample1623_50)
reduced_model_length_proceeding_50 <- lm(length_proceeding~year_decision+judge_rapporteur_id+
                                           judge1+judge2+judge3, sample1623_50)

anova(reduced_model_length_proceeding_50, full_model_length_proceeding_50)

full_model_outcome_50 <- lm(outcome~chamber_id+year_decision+judge_rapporteur_id+
                              judge1+judge2+judge3
                            , data=sample1623_50)
reduced_model_outcome_50 <- lm(outcome~year_decision+judge_rapporteur_id+
                                 judge1+judge2+judge3, sample1623_50)

anova(reduced_model_outcome_50, full_model_outcome_50)

full_model_has_popular_name_50 <- lm(has_popular_name~chamber_id+year_decision+judge_rapporteur_id+
                                       judge1+judge2+judge3
                                     , sample1623_50)
reduced_model_has_popular_name_50 <- lm(has_popular_name~year_decision+judge_rapporteur_id+
                                          judge1+judge2+judge3
                                        , sample1623_50)

anova(reduced_model_has_popular_name_50, full_model_has_popular_name_50)
# 
# full_model_length_proceeding_50 <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                          judge1+judge2+judge3+n_applicants+controversial+
#                                          n_disputed_act +
#                                          n_concerned_act + n_concerned_cact + n_topics + 
#                                          n_topicsc, sample1623_50)
# reduced_model_length_proceeding_50 <- lm(length_proceeding~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                             judge1+judge2+judge3+n_applicants+controversial+
#                                             n_disputed_act +
#                                             n_concerned_act + n_concerned_cact +  n_topics + 
#                                             n_topicsc, sample1623_50)
# 
# anova(reduced_model_length_proceeding_50, full_model_length_proceeding_50)
# 
# 
# full_model_outcome_50 <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                judge1+judge2+judge3+n_applicants+controversial+
#                                n_disputed_act +
#                                n_concerned_act + n_concerned_cact  + n_topics +
#                                n_topicsc, sample1623_50)
# reduced_model_outcome_50 <- lm(outcome~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                   judge1+judge2+judge3+n_applicants+controversial+
#                                   n_disputed_act +
#                                   n_concerned_act + n_concerned_cact + n_topics +
#                                   n_topicsc, sample1623_50)
# 
# anova(reduced_model_outcome_50, full_model_outcome_50)
# 
# full_model_has_popular_name_50 <- lm(has_popular_name~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                         judge1+judge2+judge3+n_applicants+controversial+
#                                         n_disputed_act +
#                                         n_concerned_act + n_concerned_cact + n_topics + 
#                                         n_topicsc, sample1623_50)
# reduced_model_has_popular_name_50 <- lm(has_popular_name~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                            judge1+judge2+judge3+n_applicants+
#                                            n_disputed_act +controversial+
#                                            n_concerned_act + n_concerned_cact +  n_topics + 
#                                            n_topicsc, sample1623_50)
# 
# anova(reduced_model_has_popular_name_50, full_model_has_popular_name_50)


# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding_50)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

sample1623CE_50 <- left_join(sample1623_50,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome_50)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

sample1623CE_50 <- left_join(sample1623CE_50,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_has_popular_name_50)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_hpn <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_hpn = chamber_effects, row.names= NULL
)

sample1623CE_50 <- left_join(sample1623CE_50,chamber_effects_hpn, by="chamber_id")



# Regressing the chamber fixed effect on the characteristics of the chamber
sample1623CE_50<- sample1623CE_50 %>% mutate(average_yob=(judge_yob+judge_yob+judge_yob3)/3,
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
  ungroup()

length_proceeding_50 <- lm(FE_lp~ year_decision+average_yob+ var_yob +background+uni+gender, sample1623CE_50)
summary(length_proceeding_50)

outcome_50 <- lm(FE_o ~  year_decision+average_yob+ var_yob +background+uni+gender,  sample1623CE_50)
summary(outcome_50)

has_popular_name_50 <- lm(FE_hpn~  year_decision+average_yob+ var_yob +background+uni+gender,  sample1623CE_50)
summary(has_popular_name_50)

length_proceeding_C_50 <- lm(length_proceeding~     year_decision+n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +background+uni+gender, sample1623CE_50)
summary(length_proceeding_C_50)

outcome_C_50 <- lm(outcome~ year_decision+n_applicants+
                     n_disputed_act +controversial+
                     n_concerned_act + n_concerned_cact + n_topics + 
                     + average_yob+ var_yob +background+uni+gender, sample1623CE_50)
summary(outcome_C_50)

has_popular_name_C_50 <- lm(has_popular_name~ year_decision+n_applicants+
                              n_disputed_act +controversial+
                              n_concerned_act + n_concerned_cact + n_topics + 
                              + average_yob+ var_yob +background+uni+gender,  sample1623CE_50)
summary(has_popular_name_C_50)

length_proceeding_CF_50 <- lm(length_proceeding~  year_decision+   n_applicants+judge_rapporteur_id+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + n_topics + 
                                + average_yob+ var_yob +background+uni+gender, sample1623CE_50)
summary(length_proceeding_CF_50)


outcome_CF_50 <- lm(outcome~ year_decision+n_applicants+judge_rapporteur_id+
                      n_disputed_act +controversial+
                      n_concerned_act + n_concerned_cact + n_topics + 
                      + average_yob+ var_yob +background+uni+gender,  sample1623CE_50)
summary(outcome_CF_50)

has_popular_name_CF_50 <- lm(has_popular_name~ year_decision+n_applicants+judge_rapporteur_id+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +background+uni+gender, sample1623CE_50)
summary(has_popular_name_CF_50)




sample1623_20 <- sample1623 %>%
  group_by(chamber_id) %>%            
  filter(n() > 20) %>%                
  ungroup()  
n_chambers <- sample1623_20 %>% summarise(n_distinct(chamber_id))

full_model_length_proceeding_20 <- lm(length_proceeding~chamber_id+year_decision+judge_rapporteur_id+
                                        judge1+judge2+judge3
                                      , sample1623_20)
reduced_model_length_proceeding_20 <- lm(length_proceeding~year_decision+judge_rapporteur_id+
                                           judge1+judge2+judge3, sample1623_20)

anova(reduced_model_length_proceeding_20, full_model_length_proceeding_20)

full_model_outcome_20 <- lm(outcome~chamber_id+year_decision+judge_rapporteur_id+
                              judge1+judge2+judge3
                            , data=sample1623_20)
reduced_model_outcome_20 <- lm(outcome~year_decision+judge_rapporteur_id+
                                 judge1+judge2+judge3, sample1623_20)

anova(reduced_model_outcome_20, full_model_outcome_20)

full_model_has_popular_name_20 <- lm(has_popular_name~chamber_id+year_decision+judge_rapporteur_id+
                                       judge1+judge2+judge3
                                     , sample1623_20)
reduced_model_has_popular_name_20 <- lm(has_popular_name~year_decision+judge_rapporteur_id+
                                          judge1+judge2+judge3
                                        , sample1623_20)

anova(reduced_model_has_popular_name_20, full_model_has_popular_name_20)
# 
# full_model_length_proceeding_20 <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                         judge1+judge2+judge3+n_applicants+controversial+
#                                         n_disputed_act +
#                                         n_concerned_act + n_concerned_cact + n_topics + 
#                                         n_topicsc, sample1623_20)
# reduced_model_length_proceeding_20 <- lm(length_proceeding~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                            judge1+judge2+judge3+n_applicants+controversial+
#                                            n_disputed_act +
#                                            n_concerned_act + n_concerned_cact +  n_topics + 
#                                            n_topicsc, sample1623_20)
# 
# anova(reduced_model_length_proceeding_20, full_model_length_proceeding_20)
# 
# 
# full_model_outcome_20 <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                               judge1+judge2+judge3+n_applicants+controversial+
#                               n_disputed_act +
#                               n_concerned_act + n_concerned_cact  + n_topics +
#                               n_topicsc, sample1623_20)
# reduced_model_outcome_20 <- lm(outcome~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                  judge1+judge2+judge3+n_applicants+controversial+
#                                  n_disputed_act +
#                                  n_concerned_act + n_concerned_cact + n_topics +
#                                  n_topicsc, sample1623_20)
# 
# anova(reduced_model_outcome_20, full_model_outcome_20)
# 
# full_model_has_popular_name_20 <- lm(has_popular_name~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                        judge1+judge2+judge3+n_applicants+controversial+
#                                        n_disputed_act +
#                                        n_concerned_act + n_concerned_cact + n_topics + 
#                                        n_topicsc, sample1623_20)
# reduced_model_has_popular_name_20 <- lm(has_popular_name~year_decision+type_proceedings+importance+judge_rapporteur_id+
#                                           judge1+judge2+judge3+n_applicants+
#                                           n_disputed_act +controversial+
#                                           n_concerned_act + n_concerned_cact +  n_topics + 
#                                           n_topicsc, sample1623_20)
# 
# anova(reduced_model_has_popular_name_20, full_model_has_popular_name_20)
# 

# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding_20)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

sample1623CE_20 <- left_join(sample1623_20,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome_20)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

sample1623CE_20 <- left_join(sample1623CE_20,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_has_popular_name_20)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_hpn <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_hpn = chamber_effects, row.names= NULL
)

sample1623CE_20 <- left_join(sample1623CE_20,chamber_effects_hpn, by="chamber_id")



# Regressing the chamber fixed effect on the characteristics of the chamber
sample1623CE_20<- sample1623CE_20 %>% mutate(average_yob=(judge_yob+judge_yob+judge_yob3)/3,
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
  ungroup()

length_proceeding_20 <- lm(FE_lp~ year_decision+average_yob+ var_yob +background+uni+gender, sample1623CE_20)
summary(length_proceeding_20)

outcome_20 <- lm(FE_o ~  year_decision+average_yob+ var_yob +background+uni+gender,  sample1623CE_20)
summary(outcome_20)

has_popular_name_20 <- lm(FE_hpn~  year_decision+average_yob+ var_yob +background+uni+gender,  sample1623CE_20)
summary(has_popular_name_20)

length_proceeding_C_20 <- lm(length_proceeding~     year_decision+n_applicants+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +background+uni+gender, sample1623CE_20)
summary(length_proceeding_C_20)

outcome_C_20 <- lm(outcome~ year_decision+n_applicants+
                     n_disputed_act +controversial+
                     n_concerned_act + n_concerned_cact + n_topics + 
                     + average_yob+ var_yob +background+uni+gender, sample1623CE_20)
summary(outcome_C_20)

has_popular_name_C_20 <- lm(has_popular_name~ year_decision+n_applicants+
                              n_disputed_act +controversial+
                              n_concerned_act + n_concerned_cact + n_topics + 
                              + average_yob+ var_yob +background+uni+gender,  sample1623CE_20)
summary(has_popular_name_C_20)

length_proceeding_CF_20 <- lm(length_proceeding~  year_decision+   n_applicants+judge_rapporteur_id+
                                n_disputed_act +controversial+
                                n_concerned_act + n_concerned_cact + n_topics + 
                                + average_yob+ var_yob +background+uni+gender, sample1623CE_20)
summary(length_proceeding_CF_20)


outcome_CF_20 <- lm(outcome~ year_decision+n_applicants+judge_rapporteur_id+
                      n_disputed_act +controversial+
                      n_concerned_act + n_concerned_cact + n_topics + 
                      + average_yob+ var_yob +background+uni+gender,  sample1623CE_20)
summary(outcome_CF_20)

has_popular_name_CF_20 <- lm(has_popular_name~ year_decision+n_applicants+judge_rapporteur_id+
                               n_disputed_act +controversial+
                               n_concerned_act + n_concerned_cact + n_topics + 
                               + average_yob+ var_yob +background+uni+gender, sample1623CE_20)
summary(has_popular_name_CF_20)




stargazer(length_proceeding_100,length_proceeding_C_100,
          length_proceeding_50, length_proceeding_C_50,
          length_proceeding_20, length_proceeding_C_20,omit=c(
            "^year","^judge", "^n", "controversial", "Constant"))

stargazer(outcome_100,outcome_C_100,
          outcome_50, outcome_C_50,
          outcome_20, outcome_C_20,omit=c(
            "^year","^judge", "^n", "controversial", "Constant"))

stargazer(has_popular_name_100,has_popular_name_C_100,
          has_popular_name_50, has_popular_name_C_50,
          has_popular_name_20, has_popular_name_C_20,omit=c(
            "^year","^judge", "^n", "controversial", "Constant")) 
