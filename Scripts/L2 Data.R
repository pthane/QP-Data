# Run packages
library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 999)

#Preparation of data
## Load database
EPT_Aspect_Data = read_csv("./CSV Files/L2 Learners/EPT Preterit Standardized L2 Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/L2 Learners/FCT Preterit Standardized L2 Data.csv")
EPT_Mood_Data = read_csv("./CSV Files/L2 Learners/EPT Subjunctive Standardized L2 Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/L2 Learners/FCT Subjunctive Standardized L2 Data.csv")

# Aspect GLMMs
## Aspect production
Aspect_Production = lmer(
  Average ~ FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + DELE_Std:FofA_Std + Token_Lemma_Std:Type_Main + DELE_Std:Token_Lemma_Std + DELE_Std:Type_Main + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Group_No) + (1 | Item),
  data = EPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)

## Aspect comprehension
Aspect_Comprehension = lmer(
  Average ~ FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + DELE_Std:FofA_Std + Token_Lemma_Std:Type_Main + DELE_Std:Token_Lemma_Std + DELE_Std:Type_Main + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Group_No) + (1 | Item),
  data = FCT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)


# Mood GLMMs
## Mood production
Mood_Production = lmer(
  Average ~ FofA_Std + Token_Main_Lemma_Std + DELE_Std + FofA_Std + DELE_Std:FofA_Std + Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + FofA_Std:Token_Main_Lemma_Std +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)

## Mood comprehension
Mood_Comprehension = lmer(
  Average ~ FofA_Std + Token_Main_Lemma_Std + DELE_Std + FofA_Std + DELE_Std:FofA_Std + Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + FofA_Std:Token_Main_Lemma_Std +
    (1 | Group_No) + (1 | Item),
  data = FCT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)