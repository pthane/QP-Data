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
EPT_Aspect_Data = read_csv("./CSV Files/Comparison/EPT Preterit Standardized Comparison Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/Comparison/FCT Preterit Standardized Comparison Data.csv")
Composite_Aspect_Data = read_csv("./CSV Files/Comparison/Composite Preterit Standardized Comparison Data.csv")
EPT_Mood_Data = read_csv("./CSV Files/Comparison/EPT Subjunctive Standardized Comparison Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/Comparison/FCT Subjunctive Standardized Comparison Data.csv")
Composite_Mood_Data = read_csv("./CSV Files/Comparison/Composite Subjunctive Standardized Comparison Data.csv")

# Aspect GLMMs
## Aspect production
Aspect_Production_Lemma = lmer(
  Average ~ FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Type_Main + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = EPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Production_Differential = lmer(
  Average ~ FofA_Std + Token_Differential_Std + Type_Main + DELE_Std + Token_Differential_Std:Type_Main + FofA_Std:Token_Differential_Std + FofA_Std:Type_Main + DELE_Std:Token_Differential_Std:Type_Main + FofA_Std:Token_Differential_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = EPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Production_Preterit = lmer(
  Average ~ FofA_Std + Token_Pret_Std + Type_Main + DELE_Std + Token_Pret_Std:Type_Main + FofA_Std:Token_Pret_Std + FofA_Std:Type_Main + DELE_Std:Token_Pret_Std:Type_Main + FofA_Std:Token_Pret_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = EPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production_Lemma)
summary(Aspect_Production_Differential)
summary(Aspect_Production_Preterit)


## Aspect comprehension
Aspect_Comprehension_Lemma = lmer(
  Average ~ FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Type_Main + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = FCT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Comprehension_Differential = lmer(
  Average ~ FofA_Std + Token_Differential_Std + Type_Main + DELE_Std + Token_Differential_Std:Type_Main + FofA_Std:Token_Differential_Std + FofA_Std:Type_Main + DELE_Std:Token_Differential_Std:Type_Main + FofA_Std:Token_Differential_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = FCT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Comprehension_Preterit = lmer(
  Average ~ FofA_Std + Token_Pret_Std + Type_Main + DELE_Std + Token_Pret_Std:Type_Main + FofA_Std:Token_Pret_Std + FofA_Std:Type_Main + DELE_Std:Token_Pret_Std:Type_Main + FofA_Std:Token_Pret_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = FCT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension_Lemma)
summary(Aspect_Comprehension_Differential)
summary(Aspect_Comprehension_Preterit)


## Aspect composite
Aspect_Composite_Lemma = lmer(
  Average ~ FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Type_Main + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = Composite_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Composite_Differential = lmer(
  Average ~ FofA_Std + Token_Differential_Std + Type_Main + DELE_Std + Token_Differential_Std:Type_Main + FofA_Std:Token_Differential_Std + FofA_Std:Type_Main + DELE_Std:Token_Differential_Std:Type_Main + FofA_Std:Token_Differential_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = Composite_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Composite_Preterit = lmer(
  Average ~ FofA_Std + Token_Pret_Std + Type_Main + DELE_Std + Token_Pret_Std:Type_Main + FofA_Std:Token_Pret_Std + FofA_Std:Type_Main + DELE_Std:Token_Pret_Std:Type_Main + FofA_Std:Token_Pret_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = Composite_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Composite_Lemma)
summary(Aspect_Composite_Differential)
summary(Aspect_Composite_Preterit)


# Mood GLMMs
## Mood production
Mood_Production_Matrix = lmer(
  Average ~ FofA_Std + Token_Main_Lemma_Lemma_Std + DELE_Std + FofA_Std + Token_Main_Lemma_Lemma_Std + DELE_Std:Token_Main_Lemma_Lemma_Std + FofA_Std:Token_Main_Lemma_Lemma_Std +
    (1 | Participant) + (1 | Item),
  data = EPT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production_Matrix)

## Mood comprehension
Mood_Comprehension_Matrix = lmer(
  Average ~ FofA_Std + Token_Main_Lemma_Std + DELE_Std + FofA_Std + Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + FofA_Std:Token_Main_Lemma_Std +
    (1 | Participant) + (1 | Item),
  data = FCT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension_Matrix)


## Mood composite
Mood_Composite_Matrix = lmer(
  Average ~ FofA_Std + Token_Main_Lemma_Std + DELE_Std + FofA_Std + Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + FofA_Std:Token_Main_Lemma_Std +
    (1 | Participant) + (1 | Item),
  data = Composite_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Composite_Matrix)


# Mood GLMMs for subordinate clause
## Mood production
Mood_Production_Subordinate = lmer(
  Average ~ FofA_Std + Token_Sub_Std + DELE_Std + FofA_Std + Token_Sub_Std + DELE_Std:Token_Sub_Std + FofA_Std:Token_Sub_Std +
    (1 | Participant) + (1 | Item),
  data = EPT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production_Subordinate)

## Mood comprehension
Mood_Comprehension_Subordinate = lmer(
  Average ~ FofA_Std + Token_Sub_Std + DELE_Std + FofA_Std + Token_Sub_Std + DELE_Std:Token_Sub_Std + FofA_Std:Token_Sub_Std +
    (1 | Participant) + (1 | Item),
  data = FCT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension_Subordinate)


## Mood composite
Mood_Composite_Subordinate = lmer(
  Average ~ FofA_Std + Token_Sub_Std + DELE_Std + FofA_Std + Token_Sub_Std + DELE_Std:Token_Sub_Std + FofA_Std:Token_Sub_Std +
    (1 | Participant) + (1 | Item),
  data = Composite_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Composite_Subordinate)