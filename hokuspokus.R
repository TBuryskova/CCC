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

ccc_compositions <- read_rds("../data/rds/ccc_compositions.rds")
ccc_disputed_acts <- read_rds("../data/rds/ccc_disputed_acts.rds")
ccc_judges <- read_rds("../data/rds/ccc_judges.rds")
ccc_parties <- read_rds("../data/rds/ccc_parties.rds")
ccc_separate_opinions <- read_rds("../data/rds/ccc_separate_opinions.rds")
ccc_subject_matter <- read_rds("../data/rds/ccc_subject_matter.rds")
ccc_verdicts <- read_rds("../data/rds/ccc_verdicts.rds")
ccc_metadata <- read_rds("../data/rds/ccc_metadata.rds")

controversial_topics <- c(
  "diskriminace",
  "vyvlastn\u011Bn\u00ED",
  "restituce",
  "c\u00EDrkev",
  "sexu\u00E1ln\u00ED orientace",
  "ochrana spot\u0159ebitele",
  "základn\u00ED lidsk\u00E1 pr\u00E1va",
  "soci\u00E1ln\u00ED a kulturn\u00ED práva",
  "vlastnické pr\u00E1vo",
  "svoboda projevu",
  "c\u00EDrkve"
)

data_basic <- ccc_metadata %>%
  filter(formation!="Plenum") %>% # filter away plenary decisions
  rowwise() %>%
  mutate(
    chamber_id = paste(
      sort(composition$judge_id[1:3]), # Extract the first three `judge_id`s
      collapse = ""
    ),
    judge1 = sort(composition$judge_id[1:3])[1],
    judge2 = sort(composition$judge_id[1:3])[2],
    judge3 = sort(composition$judge_id[1:3])[3],
    n_applicants= length(applicant),
    type_verdict1=type_verdict[1],
    type_verdict2=type_verdict[2],
    type_verdict3=type_verdict[3],
    final_verdict=type_verdict[[length(type_verdict)]],
    n_citations=length(citations),
    disputed_act1=disputed_act[1],
    disputed_act2=disputed_act[2],
    disputed_act3=disputed_act[3],
    n_disputed_act=length(disputed_act),
    n_concerned_act=length(concerned_acts),
    n_concerned_cact=length(concerned_constitutional_acts),
    topic1 = subject_proceedings[1],
    topic2 = subject_proceedings[2],
    topic3 = subject_proceedings[3],
    n_topics=length(subject_proceedings),
    topic1c = subject_register[1],
    topic2c = subject_register[2],
    topic3c = subject_register[3],
    n_topicsc=length(subject_register),
    meritory=(grounds=="merits"),
    controversial=any(unlist(list(case_when(
      map_lgl(subject_register, ~ any(.x %in% controversial_topics)) ~ TRUE,
      TRUE ~ FALSE
    ))))
  ) %>%  
  ungroup() %>%
  filter(str_length(chamber_id)>9) %>% # filter away cases decided only by the judge-rapporteur (rejected)
  mutate(year_submission=year(ymd(date_submission)) ) %>%
  mutate(         has_popular_name=!is.na(popular_name)) %>%
  mutate(outcome=(outcome=="granted")) %>%
  mutate(length_proceeding=case_when(length_proceeding<0 ~ NA,
                                     TRUE~length_proceeding))

case_citations_count <- ccc_metadata %>%
  unnest(citations) %>%
  group_by(citations, doc_id) %>%
  summarise(cited=n()) %>%
  rename(case_id=citations) %>%
  mutate(case_id=str_replace(case_id,"\n","")) %>%
   left_join(
    ccc_metadata %>%
      select(doc_id, case_id) %>%
      mutate(last_char = as.numeric(str_sub(doc_id, -1))) %>%
      group_by(case_id) %>%
      filter(last_char == max(last_char)) %>%
      select(-last_char)
  )

data_basic <- data_basic %>% left_join(case_citations_count, by='doc_id')

data_basic <- data_basic %>% replace_na(list(cited = 0))

judges <- ccc_judges %>% group_by(judge_id) %>%
summarise(judge_term_start=min(ymd(judge_term_start)),
          judge_term_end=max(ymd(judge_term_end)),
          judge_reelection=max(judge_reelection)) %>%
  ungroup() %>%
  left_join(ccc_judges %>% select(judge_id,judge_name,judge_yob,judge_gender, judge_uni, judge_degree, judge_profession), by=join_by(judge_id),multiple="first") %>%
  mutate(official1623=case_when(judge_id%in% c("J:41","J:40","J:42") ~ TRUE,
                                TRUE ~ FALSE))  # Tomkova, Rychetsky, Fenyk od srpna 2013 do kvetna 2023 

data_basic <- data_basic %>% 
  left_join(judges, by=c("judge1"="judge_id"), suffix=c("", "1")) %>%
  left_join(judges, by=c("judge2"="judge_id"), suffix=c("", "2")) %>%
  left_join(judges, by=c("judge3"="judge_id"), suffix=c("", "3"))

sample1623 <- data_basic %>% filter(ymd(date_submission)>=ymd("2016-01-01"),
                                    ymd(date_submission)<=ymd("2023-01-01")
) %>% 
  mutate(unresolved_rotation=case_when(ymd(date_submission)<ymd("2018-01-01") & ymd(date_decision)>ymd("2019-12-31") ~ TRUE,
                                       ymd(date_submission)<ymd("2016-01-01") & ymd(date_decision)>ymd("2017-12-31") ~ TRUE,
                                       TRUE ~ FALSE

  ),
  judge_age =  year(ymd(date_submission))-judge_yob,
  judge_age2 =  year(ymd(date_submission))-judge_yob2,
  judge_age3 =  year(ymd(date_submission))-judge_yob3,

         )
  

sample1623 <- sample1623 %>% filter(official1623==FALSE,
                                  official16232==FALSE,
                                  official16233==FALSE, !unresolved_rotation)

ggplot(sample1623, aes(x=cited)) +
  geom_histogram()



selected_vars <- c("n_applicants", "n_citations", "n_disputed_act",
                   "n_concerned_act","n_concerned_cact", "length_proceeding", "controversial", "meritory", 
                   "cited", "outcome")  # Replace with your variables

desc_stats_num <- sample1623%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.numeric, funs(mean=mean(., na.rm=TRUE)))

desc_stats_log <- sample1623%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.logical, funs(mean)) 

desc_stats <- desc_stats_num %>% cbind(desc_stats_log) %>% t()

desc_stats_num <- sample1623%>%
  select(all_of(selected_vars)) %>%
  summarise_if(is.numeric, funs(max=max(., na.rm=TRUE)
                                )) %>% t()


# Convert to LaTeX table
latex_table <- xtable(desc_stats)

# Print LaTeX code
print(latex_table, type = "latex", include.rownames = FALSE)


########### Regressions fixed effects only ##################  
full_model_length_proceeding <- lm(length_proceeding~chamber_id+year_decision+judge_rapporteur_id+
                                     judge1+judge2+judge3
                                   , sample1623)
reduced_model_length_proceeding <- lm(length_proceeding~year_decision+judge_rapporteur_id+
                                        judge1+judge2+judge3, sample1623)

anova(reduced_model_length_proceeding, full_model_length_proceeding)

full_model_outcome <- lm(outcome~chamber_id+year_decision+judge_rapporteur_id+
                           judge1+judge2+judge3
                         , data=sample1623)
reduced_model_outcome <- lm(outcome~year_decision+judge_rapporteur_id+
                              judge1+judge2+judge3, sample1623)

anova(reduced_model_outcome, full_model_outcome)

full_model_cited <- lm(cited~chamber_id+year_decision+judge_rapporteur_id+
                                    judge1+judge2+judge3
                                  , sample1623)
reduced_model_cited <- lm(cited~year_decision+judge_rapporteur_id+
                                       judge1+judge2+judge3
                                     , sample1623)

anova(reduced_model_cited, full_model_cited)

full_model_length_proceeding <- lm(length_proceeding~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
                   judge1+judge2+judge3+n_applicants+controversial+
                   n_disputed_act +
                   n_concerned_act + n_concerned_cact + n_topics + 
                  n_topicsc, sample1623)
reduced_model_length_proceeding <- lm(length_proceeding~year_decision+type_proceedings+importance+judge_rapporteur_id+
                      judge1+judge2+judge3+n_applicants+controversial+
                      n_disputed_act +
                      n_concerned_act + n_concerned_cact +  n_topics + 
                       n_topicsc, sample1623)

anova(reduced_model_length_proceeding, full_model_length_proceeding)


full_model_outcome <- lm(outcome~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
                   judge1+judge2+judge3+n_applicants+controversial+
                   n_disputed_act +
                   n_concerned_act + n_concerned_cact  + n_topics + 
                 n_topicsc, sample1623)
reduced_model_outcome <- lm(outcome~year_decision+type_proceedings+importance+judge_rapporteur_id+
                      judge1+judge2+judge3+n_applicants+controversial+
                      n_disputed_act +
                      n_concerned_act + n_concerned_cact + n_topics + 
                       n_topicsc, sample1623)

 anova(reduced_model_outcome, full_model_outcome)

full_model_cited <- lm(cited~chamber_id+year_decision+type_proceedings+importance+judge_rapporteur_id+
                           judge1+judge2+judge3+n_applicants+controversial+
                           n_disputed_act +
                           n_concerned_act + n_concerned_cact + n_topics + 
                            n_topicsc, sample1623)
reduced_model_cited <- lm(cited~year_decision+type_proceedings+importance+judge_rapporteur_id+
                              judge1+judge2+judge3+n_applicants+
                              n_disputed_act +controversial+
                              n_concerned_act + n_concerned_cact +  n_topics + 
                              n_topicsc, sample1623)

anova(reduced_model_cited, full_model_cited)


# now estimating the chamber fixed effects for each outcome and each chamber
coefficients <- coef(full_model_length_proceeding)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_lp <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_lp = chamber_effects, row.names= NULL
)

sample1623CE <- left_join(sample1623,chamber_effects_lp, by="chamber_id")

coefficients <- coef(full_model_outcome)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_o <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_o = chamber_effects, row.names= NULL
)

sample1623CE <- left_join(sample1623CE,chamber_effects_o, by="chamber_id")


coefficients <- coef(full_model_cited)
chamber_effects <- coefficients[grepl("chamber_id", names(coefficients))]
chamber_effects_cit <- data.frame(
  chamber_id = str_replace(names(chamber_effects) , "chamber_id",""),
  FE_cit = chamber_effects, row.names= NULL
)

sample1623CE <- left_join(sample1623CE,chamber_effects_cit, by="chamber_id")



# Regressing the chamber fixed effect on the characteristics of the chamber
sample1623CE<- sample1623CE %>% mutate(average_age=(judge_age+judge_age2+judge_age3)/3,
                                  var_age=(judge_age^2+judge_age2^2+judge_age3^2)/3-(judge_age+judge_age2+judge_age3)^2/9) %>%
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
  ungroup()

length_proceeding <- lm(FE_lp~ average_age+ var_age +background+uni+gender, sample1623CE)
summary(length_proceeding)

outcome <- lm(FE_o ~  average_age+ var_age +background+uni+gender,  sample1623CE)
summary(outcome)

cited <- lm(FE_cit~  average_age+ var_age +background+uni+gender,  sample1623CE)
summary(cited)

length_proceeding_C <- lm(length_proceeding~     n_applicants+
                             n_disputed_act +controversial+
                             n_concerned_act + n_concerned_cact + n_topics + 
                             + average_age+ var_age +background+uni+gender, sample1623CE)
summary(length_proceeding_C)

outcome_C <- lm(outcome~ n_applicants+
                   n_disputed_act +controversial+
                   n_concerned_act + n_concerned_cact + n_topics + 
                   + average_age+ var_age +background+uni+gender, sample1623CE)
summary(outcome_C)

cited_C <- lm(cited~ n_applicants+
                            n_disputed_act +controversial+
                            n_concerned_act + n_concerned_cact + n_topics + 
                            + average_age+ var_age +background+uni+gender,  sample1623CE)
summary(cited_C)

length_proceeding_CF <- lm(length_proceeding~     n_applicants+judge_rapporteur_id+
                                     n_disputed_act +controversial+
                                     n_concerned_act + n_concerned_cact + n_topics + 
                                    + average_age+ var_age +background+uni+gender, sample1623CE)
summary(length_proceeding_CF)

meritory_CF <- lm(meritory~     n_applicants+judge_rapporteur_id+
                             n_disputed_act +controversial+
                             n_concerned_act + n_concerned_cact + n_topics + 
                             + average_age+ var_age +background+uni+gender, sample1623CE)
summary(meritory_CF)

outcome_CF <- lm(outcome~ n_applicants+judge_rapporteur_id+
                n_disputed_act +controversial+
                n_concerned_act + n_concerned_cact + n_topics + 
                + average_age+ var_age +background+uni+gender,  sample1623CE)
summary(outcome_CF)

cited_CF <- lm(cited~ n_applicants+judge_rapporteur_id+
                         n_disputed_act +controversial+
                         n_concerned_act + n_concerned_cact + n_topics + 
                         + average_age+ var_age +background+uni+gender, sample1623CE)
summary(cited_CF)

stargazer(
          length_proceeding_C,outcome_C,cited_C,
          length_proceeding_CF,outcome_CF,cited_CF, omit=c("^judge", "^n", "controversial"))


########### chamber_id balance ########
balance <- sample1623%>% group_by(chamber_id) %>% filter(n()>500) %>%
  summarise(
    mean_n_concerned_act=  mean(n_concerned_act),
    lCI_n_concerned_act= mean(n_concerned_act)-qt(0.975, df = n() - 1) * sd(n_concerned_act) / sqrt(n()),
    uCI_n_concerned_act= mean(n_concerned_act)+qt(0.975, df = n() - 1) * sd(n_concerned_act) / sqrt(n()),
    mean_n_concerned_cact=  mean(n_concerned_cact),
    lCI_n_concerned_cact= mean(n_concerned_cact)-qt(0.975, df = n() - 1) * sd(n_concerned_cact) / sqrt(n()),
    uCI_n_concerned_cact= mean(n_concerned_cact)+qt(0.975, df = n() - 1) * sd(n_concerned_cact) / sqrt(n()),
    mean_n_citations=  mean(n_citations),
    lCI_n_citations= mean(n_citations)-qt(0.975, df = n() - 1) * sd(n_citations) / sqrt(n()),
    uCI_n_citations= mean(n_citations)+qt(0.975, df = n() - 1) * sd(n_citations) / sqrt(n()),
    mean_controversial=  mean(controversial),
    lCI_controversial= mean(controversial)-qt(0.975, df = n() - 1) * sd(controversial) / sqrt(n()),
    uCI_controversial= mean(controversial)+qt(0.975, df = n() - 1) * sd(controversial) / sqrt(n()),
    mean_meritory=  mean(meritory),
    lCI_meritory= mean(meritory)-qt(0.975, df = n() - 1) * sd(meritory) / sqrt(n()),
    uCI_meritory= mean(meritory)+qt(0.975, df = n() - 1) * sd(meritory) / sqrt(n())
  )

ggplot(balance, aes(x=chamber_id, y=mean_n_concerned_act)) +
  geom_col(fill="skyblue") +
  labs(
    y = "Number of Concerned Acts",
    x = "Chamber"
  ) + 
  geom_errorbar(aes(ymin = lCI_n_concerned_act, ymax = uCI_n_concerned_act))  +
  theme( text=element_text(size=20),
         axis.text.x = element_text(size=10),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.background = element_blank())

ggplot(balance, aes(x=chamber_id, y=mean_n_concerned_cact)) +
  geom_col(fill="skyblue") +
  labs(
    y = "Number of Concerned Constitutional Acts",
    x = "Chamber"
  ) + 
  geom_errorbar(aes(ymin = lCI_n_concerned_cact, ymax = uCI_n_concerned_cact))  +
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=10),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.background = element_blank())

ggplot(balance, aes(x=chamber_id, y=mean_controversial)) +
  geom_col(fill="skyblue") +
  geom_errorbar(aes(ymin = lCI_controversial, ymax = uCI_controversial)) +
  labs(
    y = "Proportion of Controversial Cases",
    x = "Chamber"
  ) +
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=10),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
 

balance <- sample1623%>% group_by(chamber_id)  %>% filter(n()>500) %>%
 ungroup()


manova(cbind(n_concerned_act,n_concerned_cact,controversial)~chamber_id, balance) %>% summary()

aov(n_concerned_act ~ chamber_id, balance) %>% summary()
aov(n_concerned_cact ~ chamber_id, balance) %>% summary()
aov(controversial ~ chamber_id, balance) %>% summary()

balance <- sample1623%>% group_by(chamber_id) %>%
  summarise(n=n())


ggplot(balance, aes(n)) +
  geom_histogram( binwidth=5, fill = "skyblue", color = "black") +
  
  labs(
    x = "Number of Cases",
    y = "Number of Chambers"
  ) +
  theme( text=element_text(size=20),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggplot(balance, aes(chamber_id, n)) +
  geom_col( fill = "skyblue", color = "black") +
  
  labs(
    x = "Chambers",
    y = "Number of Cases"
  ) +
  theme( text=element_text(size=20),
         axis.text.x = element_blank(),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.background = element_blank())
