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
CEPT_Aspect = read_csv("./CSV Files/Comparison/CEPT Preterit Raw Comparison Data.csv")
FCT_Aspect = read_csv("./CSV Files/Comparison/FCT Preterit Raw Comparison Data.csv")


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


# Write new CSV files
write_csv(CEPT_Aspect_Standardized, "./CSV Files/Comparison/CEPT Preterit Standardized Comparison Data.csv")
write_csv(FCT_Aspect_Standardized, "./CSV Files/Comparison/FCT Preterit Standardized Comparison Data.csv")
write_csv(Composite_Aspect_Standardized, "./CSV Files/Comparison/Composite Preterit Standardized Comparison Data.csv")
