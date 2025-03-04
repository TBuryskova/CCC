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

full_model_length_proceeding <- lm(length_proceeding~judge_1+judge2+judge3+year_decision
                                   , data_clean)

full_model_outcome <- lm(outcome~judge_1+judge2+judge3+year_decision
                         , data=data_clean)

full_model_cited_per_year <- lm(cited_per_year~judge_1+judge2+judge3+year_decision
                                , data_clean)
# now estimating the individual fixed effects for each outcome and each chamber
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
