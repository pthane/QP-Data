# Run packages
library(tidyverse)
library(effects)
library(dplyr)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 999)

# Preparation of data
Lexical_Item_Report = read.csv("./CSV Files/Lexical Item Analysis/Self-Reported Use Raw Data.csv")

state_v <- c("amar", "")


Lexical_Frequencies = Lexical_Item_Report %>% 
  pivot_longer(cols = -Participant, names_to = "Item", values_to = "Rating") %>% 
  group_by(Item) %>% 
  summarize(., Mean_Frequency = mean(Rating)) %>% 
  left_join(.,
    bind_rows(
      read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>%
        select(MainVerb, Token_Main_Lemma) %>%
        distinct(MainVerb, Token_Main_Lemma) %>% 
        mutate(type = "Main", 
          Item = stringr::str_remove(MainVerb, "#0"), 
          Item = stringr::str_remove(Item, "[0-9]"), 
          Item = stringr::str_remove(Item, ": ")) %>% 
        transmute(Item, class = "state", Lemma = Token_Main_Lemma),
      read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv") %>%
        select(MainVerb, Token_Main_Lemma, SubVerb) %>%
        distinct(MainVerb, Token_Main_Lemma) %>% 
        mutate(type = "Main", 
          Item = stringr::str_remove(MainVerb, "#0"), 
          Item = stringr::str_remove(Item, "[0-9]"), 
          Item = stringr::str_remove(Item, ": ")) %>% 
        transmute(Item, class = "matrix", Lemma = Token_Main_Lemma), 
      read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>%
        select(SubVerb, Token_Sub) %>%
        distinct(SubVerb, Token_Sub) %>% 
        na.omit() %>% 
        mutate(type = "Sub", 
          Item = stringr::str_remove(SubVerb, "#0"), 
          Item = stringr::str_remove(Item, "[0-9]"), 
          Item = stringr::str_remove(Item, ": "), 
          Item = forcats::fct_recode(Item, atar = "#0atar")) %>% 
        transmute(Item, class = "sub", Lemma = Token_Sub))
    ) %>% 
  mutate(
    mean_f_std = scale(Mean_Frequency), 
    lemma_std = scale(Lemma)
  ) 
  

write_csv(Lexical_Frequencies, "./CSV Files/Lexical Item Analysis/Standardized Lexical Frequency.csv")

## Plot
Lexical_Frequencies %>% 
  ggplot(., aes(x = mean_f_std, y = log(Lemma))) + 
    geom_point() + 
    geom_smooth(method = lm) +
    labs(x = "Average participant self-rating of lexical items", y = "Log-transformed lemma frequency", title = "Correlation of HS Lexical Use and Davies (2006) Lemma Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

# Correlations
Lexical_Frequency_Correlation = lm(Lemma ~ Mean_Frequency, data = Lexical_Frequencies)
summary(Lexical_Frequency_Correlation)