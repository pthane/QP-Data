# QP Statistics prepared by Jennifer Markovits
## Data represented for subjunctive production data only

# Run packages
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)


# Load databases
EPT_Aspect = read_csv("./CSV Files/Comparison/EPT Preterit Raw Comparison Data.csv")
EPT_Mood = read_csv("./CSV Files/Comparison/EPT Subjunctive Raw Comparison Data.csv")
FCT_Aspect = read_csv("./CSV Files/Comparison/FCT Preterit Raw Comparison Data.csv")
FCT_Mood = read_csv("./CSV Files/Comparison/FCT Subjunctive Raw Comparison Data.csv")


# Aspect data
## Modify preterit production data
EPT_Aspect_Standardized = EPT_Aspect %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))



## Modify preterit comprehension data
FCT_Aspect_Standardized = FCT_Aspect %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Mood data
## Modify subjunctive production data
EPT_Mood_Standardized = EPT_Mood %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Modify subjunctive comprehension data
FCT_Mood_Standardized = FCT_Mood %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Create composite data for subjunctive
Composite_Mood_Standardized = rbind(EPT_Mood_Standardized, FCT_Mood_Standardized)
Composite_Aspect_Standardized = rbind(EPT_Aspect_Standardized, FCT_Aspect_Standardized)


# Write new CSV files
write_csv(EPT_Aspect_Standardized, "./CSV Files/Comparison/EPT Preterit Standardized Comparison Data.csv")
write_csv(FCT_Aspect_Standardized, "./CSV Files/Comparison/FCT Preterit Standardized Comparison Data.csv")
write_csv(Composite_Aspect_Standardized, "./CSV Files/Comparison/Composite Preterit Standardized Comparison Data.csv")
write_csv(EPT_Mood_Standardized, "./CSV Files/Comparison/EPT Subjunctive Standardized Comparison Data.csv")
write_csv(FCT_Mood_Standardized, "./CSV Files/Comparison/FCT Subjunctive Standardized Comparison Data.csv")
write_csv(Composite_Mood_Standardized, "./CSV Files/Comparison/Composite Subjunctive Standardized Comparison Data.csv")