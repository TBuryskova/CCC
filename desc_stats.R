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
library(tibble)
library(modelsummary)
data_clean <- read_rds("data_clean.rds")



# Step 1: Define variables
num_vars <- c("n_applicants", "n_citations", "n_disputed_act",
              "n_concerned_act", "n_concerned_cact", "length_proceeding")
log_vars <- c("controversial", "meritory", "has_popular_name", "outcome")

# Optional but helpful: ensure numeric vars are numeric
data_clean <- data_clean %>%
  mutate(across(all_of(num_vars), ~as.numeric(as.character(.))))

# Step 2: Group by doc_id and compute means per group
groups <- data_clean %>%
  group_by(doc_id) %>%
  summarise(across(all_of(c(num_vars, log_vars)),
                   mean, na.rm = TRUE),
            .groups = "drop")

# Step 3: Compute descriptive stats for numeric vars
desc_num <- groups %>%
  select(all_of(num_vars)) %>%
  summarise(across(everything(),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x,  na.rm = TRUE),
                        min  = ~min(.x, na.rm = TRUE),
                        max  = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Use regex to correctly separate variable name and stat
desc_num <- desc_num %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_pattern = "^(.*)_(mean|sd|min|max)$",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# Step 4: Compute descriptive stats for logical vars
desc_log <- groups %>%
  select(all_of(log_vars)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "mean") %>%
  mutate(sd = NA_real_, min = NA_real_, max = NA_real_)

# Step 5: Combine both parts
desc_table <- bind_rows(desc_num, desc_log) %>%
  select(variable, mean, sd, min, max)

# Step 6: Add number of groups
desc_table <- add_row(desc_table,
                      variable = "n_groups",
                      mean     = nrow(groups),
                      sd = NA, min = NA, max = NA)

# Step 7: Format output
desc_table <- desc_table %>%
  mutate(
    # Format logical variable means as percentages
    mean = if_else(variable %in% log_vars,
                   paste0(round(mean * 100, 1), "%"),
                   sprintf("%.1f", mean)),
    
    sd = if_else(is.na(sd), NA_character_, sprintf("%.1f", sd)),
    min = if_else(is.na(min), NA_character_, sprintf("%.1f", min)),
    max = if_else(is.na(max), NA_character_, sprintf("%.1f", max))
  ) %>%
  # Format n_groups to show no decimals
  mutate(mean = if_else(variable == "n_groups",
                        as.character(as.integer(as.numeric(mean))),
                        mean))

# Step 8: Print LaTeX table
modelsummary::datasummary_df(
  desc_table,
  title = "Descriptive Statistics (one observation = one doc_id)",
  output = "latex"
)
