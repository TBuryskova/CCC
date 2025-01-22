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

data_basic <- data_basic %>% left_join(case_citations_count)

data_basic <- data_basic %>% mutate(cited=case_when(!is.na(cited) ~ cited,
                                                    TRUE ~ 0))

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

saveRDS(sample1623, "sample1623.rds")


