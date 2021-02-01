# QP Statistics prepared by Jennifer Markovits
## Data represented for subjunctive production data only

# Run packages
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)


# Load databases
EPT_Aspect = read_csv("./CSV Files/L2 Learners/EPT Preterit Raw L2 Data.csv")
EPT_Mood = read_csv("./CSV Files/L2 Learners/EPT Subjunctive Raw L2 Data.csv")
FCT_Aspect = read_csv("./CSV Files/L2 Learners/FCT Preterit Raw L2 Data.csv")
FCT_Mood = read_csv("./CSV Files/L2 Learners/FCT Subjunctive Raw L2 Data.csv")


# Aspect data
## Modify preterit production data
EPT_Aspect_Standardized = EPT_Aspect %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

EPT_Aspect_Standardized = EPT_Aspect_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         Token_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Type_Std = (Type_Main - mean(Type_Main))/sd(Type_Main),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


## Modify preterit comprehension data
FCT_Aspect_Standardized = FCT_Aspect %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

FCT_Aspect_Standardized = FCT_Aspect_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         Token_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Type_Std = (Type_Main - mean(Type_Main))/sd(Type_Main),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


# Mood data
## Modify subjunctive production data
EPT_Mood_Standardized = EPT_Mood %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

EPT_Mood_Standardized = EPT_Mood_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         Token_Main_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


## Modify subjunctive comprehension data
FCT_Mood_Standardized = FCT_Mood %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

FCT_Mood_Standardized = FCT_Mood_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         Token_Main_Lemma_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


# Create composite data for subjunctive
Composite_Mood_Standardized = rbind(EPT_Mood_Standardized, FCT_Mood_Standardized)
Composite_Aspect_Standardized = rbind(EPT_Aspect_Standardized, FCT_Aspect_Standardized)


# Write new CSV files
write_csv(EPT_Aspect_Standardized, "./CSV Files/L2 Learners/EPT Preterit Standardized L2 Data.csv")
write_csv(FCT_Aspect_Standardized, "./CSV Files/L2 Learners/FCT Preterit Standardized L2 Data.csv")
write_csv(Composite_Aspect_Standardized, "./CSV Files/L2 Learners/Composite Preterit Standardized L2 Data.csv")
write_csv(EPT_Mood_Standardized, "./CSV Files/L2 Learners/EPT Subjunctive Standardized L2 Data.csv")
write_csv(FCT_Mood_Standardized, "./CSV Files/L2 Learners/FCT Subjunctive Standardized L2 Data.csv")
write_csv(Composite_Mood_Standardized, "./CSV Files/L2 Learners/Composite Subjunctive Standardized L2 Data.csv")