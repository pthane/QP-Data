library(tidyverse)
library(effects)
library(dplyr)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 999)


# Preparation of data
Lexical_Item_Report = read.csv("./CSV Files/Lexical Item Analysis/Subjunctive Lexical Item Correlation.csv")


# Standardization
Lexical_Item_Report <- Lexical_Item_Report %>% 
  mutate(Mean_Frequency_Std = (Mean_Frequency - mean(Mean_Frequency))/sd(Mean_Frequency),
         Lemma_Std = (Lemma - mean(Lemma))/sd(Lemma))


# Correlations
Lexical_Frequency_Correlation = lm(Lemma_Std ~ Mean_Frequency_Std, data = Lexical_Item_Report)
summary(Lexical_Frequency_Correlation)
broom::tidy(Lexical_Frequency_Correlation, conf.int = TRUE)


# Plot
Lexical_Item_Report %>% 
  ggplot(., aes(x = Mean_Frequency, y = log(Lemma))) + 
  geom_point() + 
  geom_smooth(method = lm) +
  labs(x = "Average participant self-rating", y = "Log-transformed lemma frequency", title = "Correlation of HS Lexical Use and Davies (2006) Lemma Frequency") +
  theme(plot.title = element_text(hjust = 0.5))