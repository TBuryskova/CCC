library(tidyverse)
library(lubridate)
library(scales)
library(stargazer)
library(xtable)

# Helper function to extract F-statistic
get_f_value <- function(aov_model) {
  summary(aov_model)[[1]]$`F value`[1] %>% round(2)
}

# Load and clean data
data_clean <- read_rds("data_clean.rds")

# =====================
# 1. Number of Applicants
# =====================
balance_applicants <- data_clean %>%
  group_by(chamber_id) %>%
  summarise(
    mean_n = mean(n_applicants, na.rm = TRUE),
    se_n = sd(n_applicants, na.rm = TRUE) / sqrt(n())
  )

f_n_applicants <- get_f_value(aov(n_applicants ~ chamber_id, data_clean))

ggplot(balance_applicants, aes(x = chamber_id, y = mean_n)) +
  geom_bar(fill = "skyblue", stat = 'identity') +
  geom_errorbar(aes(ymin = mean_n - se_n, ymax = mean_n + se_n), width = 0.2) +
  ggtitle(paste("F =", f_n_applicants)) +
  labs(y = "Number of Applicants", x = "Chamber") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave("balance_n_applicants.png")

# =====================
# 2. Number of Concerned Acts
# =====================
balance_acts <- data_clean %>%
  group_by(chamber_id) %>%
  summarise(
    mean_n = mean(n_concerned_act, na.rm = TRUE),
    se_n = sd(n_concerned_act, na.rm = TRUE) / sqrt(n())
  )

f_n_acts <- get_f_value(aov(n_concerned_act ~ chamber_id, data_clean))

ggplot(balance_acts, aes(x = chamber_id, y = mean_n)) +
  geom_bar(fill = "skyblue", stat = 'identity') +
  geom_errorbar(aes(ymin = mean_n - se_n, ymax = mean_n + se_n), width = 0.2) +
  ggtitle(paste("F =", f_n_acts)) +
  labs(y = "Number of Concerned Acts", x = "Chamber") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave("balance_n_acts.png")

# =====================
# 3. Controversial Cases
# =====================
balance_controversial <- data_clean %>%
  group_by(chamber_id) %>%
  summarise(
    mean_n = mean(controversial, na.rm = TRUE),
    se_n = sd(controversial, na.rm = TRUE) / sqrt(n())
  )

f_controversial <- get_f_value(aov(controversial ~ chamber_id, data_clean))

ggplot(balance_controversial, aes(x = chamber_id, y = mean_n)) +
  geom_bar(fill = "skyblue", stat = 'identity') +
  geom_errorbar(aes(ymin = mean_n - se_n, ymax = mean_n + se_n), width = 0.2) +
  ggtitle(paste("F =", f_controversial)) +
  labs(y = "Proportion of Controversial Cases", x = "Chamber") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave("balance_controversial.png")

# =====================
# 4. Day of the Week Submission
# =====================
dow <- data_clean %>%
  mutate(dow = wday(ymd(date_submission)))

f_dow <- get_f_value(aov(dow ~ chamber_id, dow))

ggplot(dow, aes(x = factor(dow), fill = chamber_id)) +
  geom_bar(aes(y = after_stat(prop), group = chamber_id), position = "dodge") +
  scale_x_discrete(labels = c("7" = "Sa", "1" = "Su", "2" = "Mo", "3" = "Tu", "4" = "We", "5" = "Th", "6" = "Fr")) +
  ggtitle(paste("F =", f_dow)) +
  labs(y = "Proportion", x = "Day of the Week") +
  facet_wrap(~ chamber_id, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_blank())
ggsave("balance_day_of_week.png")

# =====================
# 5. Number of Cases by Chamber
# =====================
balance_cases <- data_clean %>%
  group_by(chamber_id) %>%
  summarise(n = n())

ggplot(balance_cases, aes(chamber_id, n)) +
  geom_col() +
  labs(x = "Chambers", y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave("balance_n_cases.png")
# =====================
# 6. Average Monthly Number of Cases by Chamber
# =====================
# =====================
# 6. Average Monthly Number of Cases by Chamber
# =====================
monthly_cases <- data_clean %>%
  mutate(month = floor_date(ymd(date_submission), "month")) %>%
  group_by(chamber_id, month) %>%
  summarise(n_cases = n(), .groups = "drop")

# Compute summary stats for plotting
balance_monthly <- monthly_cases %>%
  group_by(chamber_id) %>%
  summarise(
    mean_monthly_cases = mean(n_cases, na.rm = TRUE),
    se_monthly_cases = sd(n_cases, na.rm = TRUE) / sqrt(n())
  )

# Compute F value from ANOVA
f_monthly_cases <- get_f_value(aov(n_cases ~ chamber_id, data = monthly_cases))

# Plot
ggplot(balance_monthly, aes(x = chamber_id, y = mean_monthly_cases)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_monthly_cases - se_monthly_cases, ymax = mean_monthly_cases + se_monthly_cases),
                width = 0.2) +
  ggtitle(paste("F =", f_monthly_cases)) +
  labs(
    x = "Chamber",
    y = "Average Monthly Number of Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave("balance_monthly_avg_cases.png")
