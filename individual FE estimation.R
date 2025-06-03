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

full_model_length_proceeding <- lm(length_proceeding~judge+year_submission
                                   , data_clean)

full_model_outcome <- lm(outcome~judge+year_submission
                         , data=data_clean)

full_model_cited <- lm(cited~judge+year_submission
                                , data_clean)

# now estimating the individual fixed effects for each outcome and each judge
coefficients <- coef(full_model_length_proceeding)
judge_effects <- coefficients[grepl("judge", names(coefficients))]
judge_effects_lp <- data.frame(
  judge = str_replace(names(judge_effects) , "judge",""),
  FE_lp = judge_effects, row.names= NULL
)

data_cleanJE <- left_join(data_clean,judge_effects_lp, by="judge")


coefficients <- coef(full_model_outcome)
judge_effects <- coefficients[grepl("judge", names(coefficients))]
judge_effects_o <- data.frame(
  judge = str_replace(names(judge_effects) , "judge",""),
  FE_o = judge_effects, row.names= NULL
)

data_cleanJE <- left_join(data_cleanJE,judge_effects_o, by="judge")


coefficients <- coef(full_model_cited)
judge_effects <- coefficients[grepl("judge", names(coefficients))]
judge_effects_c <- data.frame(
  judge = str_replace(names(judge_effects) , "judge",""),
  FE_c = judge_effects, row.names= NULL
)

data_cleanJE <- left_join(data_cleanJE,judge_effects_c, by="judge")


saveRDS(data_cleanJE, file="data_cleanJE.rds")



