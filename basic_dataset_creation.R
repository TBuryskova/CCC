library(dplyr) # Optional, for data manipulation if needed
library(stringr)
library(scales)
library(readr)
library(lubridate)
library(stargazer)
library(fuzzyjoin)
library(tidyr)
library(stringr)
library(tidyverse)
library(purrr)
library(xtable)
library(data.table)

ccc_compositions <- read_rds("../data/rds/ccc_compositions.rds")
ccc_judges <- read_rds("../data/rds/ccc_judges.rds")
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

floor_even <- function(x) {
  x_floor <- floor(x)
  # Return even floor if already even, else subtract 1
  ifelse(x_floor %% 2 == 0, x_floor, x_floor - 1)
}
data_basic <- ccc_metadata %>%
 filter(ymd(date_submission)>=ymd("2016-01-01"))   %>% 
  filter(formation!="Plenum") %>% # filter away plenary decisions
  rowwise() %>%
  mutate(
    case_id = str_replace_all(case_id, " ", ""),
    year_submission = year(ymd(date_submission)),  # Moved this line up
    chamber_id = paste(
      sort(composition$judge_id[1:3]),
      collapse=""
    ),
    year_even = floor_even(year_submission),
    chamber_id_alt = paste0(
      chamber_id,
      year_even
    ),
    judge1 = sort(composition$judge_id[1:3])[1],
    judge2 = sort(composition$judge_id[1:3])[2],
    judge3 = sort(composition$judge_id[1:3])[3],
    n_applicants= length(applicant),
    type_verdict1=type_verdict[1],
    type_verdict2=type_verdict[2],
    type_verdict3=type_verdict[3],
    final_verdict=type_verdict[[length(type_verdict)]],
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
  filter(str_length(chamber_id)==12) %>% # filter away cases decided only by the judge-rapporteur (rejected)
  mutate(year_submission=year(ymd(date_submission)) ) %>%
  mutate(date_submission=(ymd(date_submission)) ) %>%
  mutate(         has_popular_name=!is.na(popular_name) ,
  ) %>%
  mutate(outcome=(outcome=="granted")) %>%
  mutate(length_proceeding=case_when(length_proceeding<0 ~ NA,
                                     TRUE~length_proceeding)) %>%
  mutate(  n_citations = map_int(citations, ~ length(.x))  )

case_citations_count <- ccc_metadata %>%
  unnest(citations) %>%
  group_by(citations) %>%
  summarise(cited=n()) %>%
  rename(case_id=citations) %>%
  mutate(case_id=str_replace_all(case_id," ","")) 

data_basic <- data_basic %>% left_join(case_citations_count, by =join_by(case_id), multiple="first")

data_basic <- data_basic %>% mutate(cited=case_when(!is.na(cited) ~ cited,
                                                    TRUE ~ 0)) 
judges <- ccc_judges %>% group_by(judge_id) %>%
  summarise(judge_term_start=min(ymd(judge_term_start)),
            judge_term_end=max(ymd(judge_term_end)),
            judge_reelection=max(judge_reelection)) %>%
  ungroup() %>%
  left_join(ccc_judges %>% select(judge_id,judge_name,judge_yob,judge_gender, judge_uni, judge_degree, judge_profession), by=join_by(judge_id),multiple="first") %>%
  mutate(official1623=case_when(judge_id %in% c("J:41","J:40","J:42") ~ TRUE,
                                TRUE ~ FALSE))  # Tomkova, Rychetsky, Fenyk od srpna 2013 do kvetna 2023 


data_basic <- data_basic %>% pivot_longer(cols=c(judge1,judge2,judge3),
                                          names_to = "name", values_to="judge"
)


data_basic <- data_basic %>% 
  left_join(judges, by=c("judge"="judge_id")) 

data_basic <- data_basic %>%
  group_by(doc_id) %>%
  mutate(unresolved_rotation=case_when(ymd(date_submission)<ymd("2018-01-01") & ymd(date_decision)>ymd("2019-12-31") ~ TRUE,
                                       ymd(date_submission)<ymd("2016-01-01") & ymd(date_decision)>ymd("2017-12-31") ~ TRUE,
                                       TRUE ~ FALSE
                                       
  ),
  )  %>%
  mutate(average_yob=mean(judge_yob),
         var_yob=var(judge_yob) )%>%
  mutate(background = paste0(sort(str_sub(judge_profession, 1, 1)), collapse = ""),
         uni = paste0(sort(str_sub(judge_uni, 1, 1)), collapse = ""),
         gender = paste0(sort(str_sub(judge_gender, 1, 1)), collapse = ""),
         distinct_backgrounds=length(unique(judge_profession)),
         scholar=(any(judge_profession=="scholar")),
         distinct_uni=length(unique(judge_uni))
  ) %>%
  ungroup() %>%
  mutate(same_background=(distinct_backgrounds==1),
         all_different_background=(distinct_backgrounds==3),
         same_uni=(distinct_uni==1),
         all_different_uni=(distinct_uni==3)
  ) %>%
  mutate(gender=factor(gender, levels=c("MMM","FMM", "FFM","FFF")))


#merge with webscrapped
chambers <- read.csv("../data/csv/chamber compositions.csv")
substitute <- read.csv("../data/csv/substitute chamber members.csv")


chambers <- chambers %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)) %>% 
  mutate(end_date=case_when(is.na(end_date)~ dmy("31/12/2024"),
                            TRUE ~ end_date) )%>%
  rename(formation=chamber_id) %>%
  rename(asjudge_id=judge_id) 
  

substitute <- substitute %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)) %>% 
  mutate(end_date=case_when(is.na(end_date)~ dmy("31/12/2024"),
                            TRUE ~ end_date) )%>%
  rename(formation=chamber_id) %>%
  rename(subjudge_id=judge_id) 


data_basic <- data_basic %>% 
  mutate(formation=case_when(formation=="First Chamber" ~ "1st",
                             formation=="Second Chamber" ~ "2nd",
                             formation=="Third Chamber" ~ "3rd",
                             formation=="Fourth Chamber" ~ "4th"))

chambers_daily <- chambers %>%
  mutate(date_seq = map2(start_date, end_date,
                         ~ seq(.x, .y, by = "day"))) %>%
  unnest(date_seq) %>% select(-start_date,-end_date) %>%
  mutate(formation = str_trim(formation))


substitute_daily <- substitute %>%
  mutate(date_seq = ymd(map2(start_date, end_date,
                         ~ seq(.x, .y, by = "day")))) %>%
  unnest(date_seq) %>% select(-start_date,-end_date) %>%
  mutate(formation = str_trim(formation))


decisions_with_judges <- data_basic %>%
  left_join(chambers_daily, by = c("formation", "date_submission"="date_seq")) %>%
  group_by(doc_id) %>%
  summarise(
    asjudges = list(sort(unique(asjudge_id[!is.na(asjudge_id)]))),
    asjudge1 = asjudges[[1]][1],
    asjudge2 = asjudges[[1]][2],
    asjudge3 = asjudges[[1]][3],
    .groups = "drop"
  )
# 
decisions_with_substitutes <- data_basic %>%
  left_join(substitute_daily, by = c("formation", "date_submission"="date_seq")) %>%
  group_by(doc_id) %>%
  summarise(subjudgeA = sort(unique(subjudge_id), na.last = NA)[1],
            subjudgeB = sort(unique(subjudge_id), na.last = NA)[2],
            .groups = "drop"
  ) %>%
  ungroup()
# 
data_basic <- data_basic %>%
  left_join(decisions_with_judges, by = c("doc_id"))

data_basic <- data_basic %>%
  left_join(decisions_with_substitutes, by = c("doc_id"))

data_basic <- data_basic %>% group_by(doc_id) %>%
  mutate(composition_ok = all(judge %in% unique(c(asjudge1, asjudge2, asjudge3, subjudgeA, subjudgeB)))) %>%
  ungroup()

data_clean <- data_basic %>% filter(composition_ok)

saveRDS(data_basic, "data_basic.rds")
saveRDS(data_clean, "data_clean.rds")


