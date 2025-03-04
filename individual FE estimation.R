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

full_model_length_proceeding <- lm(length_proceeding~judge1+judge2+judge3+year_decision
                                   , data_cleanCE)

full_model_outcome <- lm(outcome~judge1+judge2+judge3+year_decision
                         , data=data_cleanCE)

full_model_cited_per_year <- lm(cited_per_year~judge1+judge2+judge3+year_decision
                                , data_cleanCE)
# now estimating the individual fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding)
judge1_effects <- coefficients[grepl("judge1", names(coefficients))]
judge1_effects_lp <- data.frame(
  judge1 = str_replace(names(judge1_effects) , "judge1",""),
  FE_lpj1 = judge1_effects, row.names= NULL
)

coefficients <- coef(full_model_length_proceeding)
judge2_effects <- coefficients[grepl("judge2", names(coefficients))]
judge2_effects_lp <- data.frame(
  judge2 = str_replace(names(judge2_effects) , "judge2",""),
  FE_lpj2 = judge2_effects, row.names= NULL
)
coefficients <- coef(full_model_length_proceeding)
judge3_effects <- coefficients[grepl("judge3", names(coefficients))]
judge3_effects_lp <- data.frame(
  judge3 = str_replace(names(judge3_effects) , "judge3",""),
  FE_lpj3 = judge3_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,judge1_effects_lp, by="judge1") %>% 
  left_join(judge2_effects_lp, by="judge2") %>% 
  left_join(judge3_effects_lp, by="judge3") 

coefficients <- coef(full_model_outcome)
judge1_effects <- coefficients[grepl("judge1", names(coefficients))]
judge1_effects_o <- data.frame(
  judge1 = str_replace(names(judge1_effects) , "judge1",""),
  FE_oj1 = judge1_effects, row.names= NULL
)

coefficients <- coef(full_model_outcome)
judge2_effects <- coefficients[grepl("judge2", names(coefficients))]
judge2_effects_o <- data.frame(
  judge2 = str_replace(names(judge2_effects) , "judge2",""),
  FE_oj2 = judge2_effects, row.names= NULL
)
coefficients <- coef(full_model_outcome)
judge3_effects <- coefficients[grepl("judge3", names(coefficients))]
judge3_effects_o <- data.frame(
  judge3 = str_replace(names(judge3_effects) , "judge3",""),
  FE_oj3 = judge3_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,judge1_effects_o, by="judge1") %>% 
  left_join(judge2_effects_o, by="judge2") %>% 
  left_join(judge3_effects_o, by="judge3") 


coefficients <- coef(full_model_cited_per_year)
judge1_effects <- coefficients[grepl("judge1", names(coefficients))]
judge1_effects_cpy <- data.frame(
  judge1 = str_replace(names(judge1_effects) , "judge1",""),
  FE_cpyj1 = judge1_effects, row.names= NULL
)

coefficients <- coef(full_model_cited_per_year)
judge2_effects <- coefficients[grepl("judge2", names(coefficients))]
judge2_effects_cpy <- data.frame(
  judge2 = str_replace(names(judge2_effects) , "judge2",""),
  FE_cpyj2 = judge2_effects, row.names= NULL
)
coefficients <- coef(full_model_cited_per_year)
judge3_effects <- coefficients[grepl("judge3", names(coefficients))]
judge3_effects_cpy <- data.frame(
  judge3 = str_replace(names(judge3_effects) , "judge3",""),
  FE_cpyj3 = judge3_effects, row.names= NULL
)

data_cleanCE <- left_join(data_cleanCE,judge1_effects_cpy, by="judge1") %>% 
  left_join(judge2_effects_cpy, by="judge2") %>% 
  left_join(judge3_effects_cpy, by="judge3") 


saveRDS(data_cleanCE, file="data_cleanCE.rds")
