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

##### as subsamples #####
data_cleanCE <- read_rds("data_cleanCE.rds")
data_cleanCE_merits <- data_cleanCE %>% filter(decided_on_merits == TRUE)
data_cleanCE_nonmerits <- data_cleanCE %>% filter(decided_on_merits == FALSE)

# Regressing the chamber fixed effect on the characteristics of the chamber
data_cleanCE_merits <- data_cleanCE_merits  %>% group_by(chamber_id_alt) %>%
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

# Regressing the chamber fixed effect on the characteristics of the chamber
data_cleanCE_nonmerits <- data_cleanCE_nonmerits  %>% group_by(chamber_id_alt) %>%
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

model_lp_merits <- lm(FE_lp ~ gender + scholar + same_uni +  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_merits)
model_lp_nonmerits <- lm(FE_lp ~ gender + scholar + same_uni +  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_nonmerits)

model_o_merits <- lm(FE_o ~ gender + scholar + same_uni +  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_merits)
model_o_nonmerits <- lm(FE_o ~ gender + scholar + same_uni +  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_nonmerits)


model_c_merits <- lm(FE_c ~ gender + scholar + same_uni +  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_merits)
model_c_nonmerits <- lm(FE_c ~ gender + scholar + same_uni +  average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE_nonmerits)


stargazer(model_lp_merits, model_lp_nonmerits,model_o_merits,model_o_nonmerits, model_c_merits,model_c_nonmerits,
          omit = c("^year", "^judge", "^n", "controversial","^grounds", "Constant"))

##### as outcomes #####
data_cleanCE <- read_rds("data_cleanCE.rds")


# Regressing the chamber fixed effect on the characteristics of the chamber
data_cleanCE<- data_cleanCE %>% group_by(chamber_id_alt) %>%
  summarize(
    gender=first(gender), uni=first(uni), background=first(background), var_yob=max(var_yob),average_yob=max(average_yob),
    length_proceeding=mean(length_proceeding),n_applicants=mean(n_applicants), n_disputed_act=mean(n_disputed_act),
    controversial=mean(controversial),n_concerned_act=mean(n_concerned_act),n_concerned_cact=mean(n_concerned_cact),n_topics=mean(n_topics),
    FE_m=mean(FE_m),
    outcome=mean(outcome), cited=mean(cited) ,
    same_background=mean(same_background),
    all_different_background=mean(all_different_background),
    same_uni=mean(same_uni), scholar=mean(scholar)
  )


merits <- lm(FE_m~ average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE)
summary(merits)


merits_C <- lm(FE_m~    n_applicants+
                            n_disputed_act +controversial+
                            n_concerned_act + n_concerned_cact + 
                            + average_yob+ var_yob +same_background+all_different_background+scholar+same_uni+gender, data_cleanCE)
summary(merits_C)

stargazer(merits, merits_C,
          omit = c("^year", "^judge", "^n", "controversial","^grounds", "Constant"))
