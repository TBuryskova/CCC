############################################################
## Descriptive-statistics table for data_clean & data_basic
############################################################

## ---- 0. Packages --------------------------------------------------------
library(dplyr)          # data manipulation
library(tidyr)          # reshaping
library(purrr)          # map functions
library(readr)          # read_rds()
library(lubridate)      # dates
library(stringr)        # string helpers
library(xtable)         # pretty LaTeX tables

## ---- 1. Read data -------------------------------------------------------
data_clean <- read_rds("data_clean.rds")
data_basic <- read_rds("data_basic.rds")

## ---- 2. Variable lists --------------------------------------------------
num_vars <- c("n_applicants", "n_citations", "n_disputed_act",
              "n_concerned_act", "n_concerned_cact", "length_proceeding")

log_vars <- c("controversial", "meritory", "has_popular_name", "outcome")

extra_count_rows <- c("n_groups", "n_chambers", "n_judges")

## ---- 3. Helper that returns a ready-to-print table ----------------------
make_desc_table <- function(df) {
  
  ## 3a. force numerics (helps if something is read as character)
  df <- df %>%
    mutate(across(all_of(num_vars), ~ as.numeric(as.character(.))))
  
  ## 3b. group by doc_id (one observation per decision)
  groups <- df %>%
    group_by(doc_id) %>%
    summarise(across(all_of(c(num_vars, log_vars)), mean, na.rm = TRUE),
              .groups = "drop")
  
  ## 3c. numeric variables: mean / sd / min / max
  desc_num <- groups %>%
    summarise(across(all_of(num_vars),
                     list(mean = ~ mean(.x, na.rm = TRUE),
                          sd   = ~ sd(.x,  na.rm = TRUE),
                          min  = ~ min(.x, na.rm = TRUE),
                          max  = ~ max(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}")) %>%
    pivot_longer(everything(),
                 names_to   = c("variable", "stat"),
                 names_pattern = "^(.*)_(mean|sd|min|max)$",
                 values_to  = "value") %>%
    pivot_wider(names_from = stat, values_from = value)
  
  ## 3d. logical variables: share = mean
  desc_log <- groups %>%
    summarise(across(all_of(log_vars), mean, na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "mean") %>%
    mutate(sd = NA_real_, min = NA_real_, max = NA_real_)
  
  ## 3e. combine
  desc <- bind_rows(desc_num, desc_log) %>%
    select(variable, mean, sd, min, max)
  
  ## 3f. add counts --------------------------------------------------------
  desc <- desc %>%
    add_row(variable = "n_groups",    mean = nrow(groups)) %>%
    add_row(variable = "n_chambers",  mean = n_distinct(df$chamber_id)) %>%
    add_row(variable = "n_judges",    mean = n_distinct(df$judge_name))
  
  ## 3g. pretty-print numbers ---------------------------------------------
  desc <- desc %>%
    mutate(
      mean = case_when(
        variable %in% log_vars             ~ sprintf("%.1f%%", mean * 100),      # percents
        variable %in% extra_count_rows     ~ as.character(round(mean)),          # integers
        TRUE                               ~ sprintf("%.1f", mean)
      ),
      sd  = ifelse(is.na(sd),  "", sprintf("%.1f", sd)),
      min = ifelse(is.na(min), "", sprintf("%.1f", min)),
      max = ifelse(is.na(max), "", sprintf("%.1f", max))
    )
  
  desc
}

## ---- 4. Build tables for both datasets ----------------------------------
table_clean <- make_desc_table(data_clean)
table_basic <- make_desc_table(data_basic)

## ---- 5. Merge horizontally ---------------------------------------------
combined <- full_join(table_clean, table_basic, by = "variable", suffix = c("_clean", "_basic")) %>%
  select(variable,
         mean_clean,  sd_clean,  min_clean,  max_clean,
         mean_basic, sd_basic, min_basic, max_basic)

## ---- 6. LaTeX output using xtable --------------------------------------
# Replace NAs with empty strings for LaTeX cleanliness
combined[is.na(combined)] <- ""

# Create xtable
xtab <- xtable(combined,
               caption = "Descriptive Statistics (one row = one doc\\_id)",
               label = "tab:desc_stats",
               align = c("l", rep("c", ncol(combined) )))  # first column left-aligned

# Print LaTeX table
print(xtab,
      include.rownames = FALSE,
      sanitize.text.function = identity,   # allow underscores and percent signs
      caption.placement = "top",
      floating = TRUE)
