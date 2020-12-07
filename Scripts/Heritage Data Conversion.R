# QP Statistics prepared by Jennifer Markovits
## Data represented for subjunctive production data only

# Run packages
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)


# Load databases
CEPT_Aspect = read_csv("./CSV Files/Heritage/CEPT Preterit Raw Heritage Data.csv")
CEPT_Mood = read_csv("./CSV Files/Heritage/CEPT Subjunctive Raw Heritage Data.csv")
FCT_Aspect = read_csv("./CSV Files/Heritage/FCT Preterit Raw Heritage Data.csv")
FCT_Mood = read_csv("./CSV Files/Heritage/FCT Subjunctive Raw Heritage Data.csv")


# Aspect data
## Modify preterit production data
CEPT_Aspect_Standardized = CEPT_Aspect %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

CEPT_Aspect_Standardized = CEPT_Aspect_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         LF_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         TF_Std = (TF_Main - mean(TF_Main))/sd(TF_Main),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


## Modify preterit comprehension data
FCT_Aspect_Standardized = CEPT_Aspect %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

FCT_Aspect_Standardized = FCT_Aspect_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         LF_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         TF_Std = (TF_Main - mean(TF_Main))/sd(TF_Main),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


## Create composite data for preterit
Composite_Aspect = rbind(CEPT_Aspect, FCT_Aspect)

Composite_Aspect_Standardized = Composite_Aspect %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

Composite_Aspect_Standardized = Composite_Aspect_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         LF_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


# Mood data
## Modify subjunctive production data
CEPT_Mood_Standardized = CEPT_Mood %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

CEPT_Mood_Standardized = CEPT_Mood_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(LF = LF_Main + LF_Sub) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         LF_Main_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         LF_Sub_Std = (LF_Sub - mean(LF_Sub))/sd(LF_Sub),
         LF_Std = (LF - mean(LF))/sd(LF),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


## Modify preterit comprehension data
FCT_Mood_Standardized = FCT_Mood %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

FCT_Mood_Standardized = FCT_Mood_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(LF = LF_Main + LF_Sub) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         LF_Main_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         LF_Sub_Std = (LF_Sub - mean(LF_Sub))/sd(LF_Sub),
         LF_Std = (LF - mean(LF))/sd(LF),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))

## Create composite data for subjunctive
Composite_Mood = rbind(CEPT_Mood, FCT_Mood)

Composite_Mood_Standardized = Composite_Mood %>%
  mutate(Participant = str_extract(Participant_ID, '[0-9]+'))

Composite_Mood_Standardized = Composite_Mood_Standardized %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         LF_Main_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         LF_Sub_Std = (LF_Sub - mean(LF_Sub))/sd(LF_Sub),
         LF_Std = (LF_Main - mean(LF_Main))/sd(LF_Main),
         LOR_Std = (LOR - mean(LOR))/sd(LOR),
         DELE_Std = (DELE - mean(DELE))/sd(DELE))


# Write new CSV files
write_csv(CEPT_Aspect_Standardized, "./CSV Files/Heritage/CEPT Preterit Standardized Heritage Data.csv")
write_csv(FCT_Aspect_Standardized, "./CSV Files/Heritage/FCT Preterit Standardized Heritage Data.csv")
write_csv(Composite_Aspect_Standardized, "./CSV Files/Heritage/Composite Preterit Standardized Heritage Data.csv")
write_csv(CEPT_Mood_Standardized, "./CSV Files/Heritage/CEPT Subjunctive Standardized Heritage Data.csv")
write_csv(FCT_Mood_Standardized, "./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv")
write_csv(Composite_Mood_Standardized, "./CSV Files/Heritage/Composite Subjunctive Standardized Heritage Data.csv")