# Run packages
library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 999)

# Preparation of data
## Load database
EPT_Aspect_Data = read_csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv")
EPT_Mood_Data = read_csv("./CSV Files/Comparison/Comparison EPT Subjunctive Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/Comparison/Comparison FCT Subjunctive Data.csv")

# Aspect GLMMs
## Aspect production
Aspect_Production = glmer(
  ExpForm ~ Token_Lemma_Std + FofA_Std + DELE_Std + Reg_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Reg_Main + FofA_Std:Token_Lemma_Std:Reg_Main +
    (1+FofA_Std | Group_No) + (1 | Item),
  data = EPT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)

## Aspect comprehension
Aspect_Comprehension = glmer(
  ExpForm ~ Token_Lemma_Std + FofA_Std + DELE_Std + Reg_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Reg_Main + FofA_Std:Token_Lemma_Std:Reg_Main +
    (1+FofA_Std | Group_No) + (1 | Item),
  data = FCT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)

# Mood GLMMs
## Mood production
Mood_Production = glmer(
  ExpForm ~ AoA_ENG_Std + FofA_Std + DELE_Std + Token_Lemma_Std + AoA_ENG_Std:FofA_Std + FofA_Std:Token_Lemma_Std + AoA_ENG_Std:Token_Lemma_Std +
    (1 + FofA_Std | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)

## Mood comprehension
Mood_Comprehension = glmer(
  ExpForm ~ AoA_ENG_Std + FofA_Std + DELE_Std + Token_Lemma_Std + AoA_ENG_Std:FofA_Std + FofA_Std:Token_Lemma_Std + AoA_ENG_Std:Token_Lemma_Std +
    (1 + FofA_Std | Group_No) + (1 | Item),
  data = FCT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)