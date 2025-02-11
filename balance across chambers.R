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

sample1623 <- read_rds("sample1623.rds")



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
  summarise(n_cases=n())


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
