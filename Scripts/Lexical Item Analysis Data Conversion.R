# Run packages
library(tidyverse)
library(effects)
library(dplyr)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 99)

# Preparation of data
Lexical_Item_Report = read.csv("./CSV Files/Lexical Item Analysis/Self-Reported Use Raw Data.csv")

Lexical_Frequencies = Lexical_Item_Report %>%
  pivot_longer(cols = -Participant, names_to = "Item", values_to = "Rating") %>%
  group_by(Item) %>% 
  summarize(Mean_Frequency = mean(Rating)) %>% 
  left_join(.,
    bind_rows(
      read_csv("./CSV Files/Heritage/EPT Preterit Standardized Heritage Data.csv") %>%
        select(MainVerb, Token_Main_Lemma) %>%
        distinct(MainVerb, Token_Main_Lemma) %>% 
        mutate(type = "Main") %>% 
        rename(Item = MainVerb, Lemma = Token_Main_Lemma),
      read_csv("./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv") %>%
        select(MainVerb, Token_Main_Lemma) %>%
        distinct(MainVerb, Token_Main_Lemma) %>% 
        mutate(type = "Main") %>% 
        rename(Item = MainVerb, Lemma = Token_Main_Lemma), 
      read_csv("./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv") %>%
        select(SubVerb, Token_Sub) %>%
        distinct(SubVerb, Token_Sub) %>%
        mutate(type = "Sub") %>% 
        rename(Item = SubVerb, Lemma = Token_Sub)))

write_csv(Lexical_Frequencies, "./CSV Files/Lexical Item Analysis/Standardized Lexical Frequency.csv")

## Plot
Lexical_Frequencies %>% 
  ggplot(., aes(x = Mean_Frequency, y = log(Lemma))) + 
    geom_point() + 
    geom_smooth(method = lm) +
    labs(x = "Average participant self-rating of lexical items", y = "Log-transformed lemma frequency", title = "Correlation of HS Lexical Use and Davies (2006) Lemma Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

# Correlations
Lexical_Frequency_Correlation = lm(Lemma ~ Mean_Frequency, data = Lexical_Frequencies)
summary(Lexical_Frequency_Correlation)