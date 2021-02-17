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
EPT_Aspect_Data = read_csv("./CSV Files/Heritage/EPT Preterit Standardized Heritage Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/Heritage/FCT Preterit Standardized Heritage Data.csv")
EPT_Mood_Data = read_csv("./CSV Files/Heritage/EPT Subjunctive Standardized Heritage Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv")
Composite_Aspect_Data = read_csv("./CSV Files/Heritage/Composite Preterit Standardized Heritage Data.csv")
Composite_Mood_Data = read_csv("./CSV Files/Heritage/Composite Subjunctive Standardized Heritage Data.csv")


# Aspect GLMMs
## Aspect production
Aspect_Production = glmer(
  Average ~ Token_Lemma_Std + FofA_Std + DELE_Std + Reg_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Reg_Main + FofA_Std:Token_Lemma_Std:Reg_Main +
    (1 | Group_No) + (1 | Item),
  data = EPT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)

## Aspect comprehension
Aspect_Comprehension = glmer(
  Average ~ Token_Lemma_Std + FofA_Std + DELE_Std + Reg_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Reg_Main + FofA_Std:Token_Lemma_Std:Reg_Main +
    (1 | Group_No) + (1 | Item),
  data = FCT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)

# Mood GLMMs
## Mood production with AoA
Mood_Production = glmer(
  Average ~ AoA_ENG_Std + FofA_Std + DELE_Std + Token_Main_Lemma_Std + AoA_ENG_Std:FofA_Std + FofA_Std:Token_Main_Lemma_Std + AoA_ENG_Std:Token_Main_Lemma_Std +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)


## Mood comprehension
Mood_Comprehension = glmer(
  Average ~ AoA_ENG_Std + FofA_Std + DELE_Std + Token_Main_Lemma_Std + AoA_ENG_Std:FofA_Std + FofA_Std:Token_Main_Lemma_Std + AoA_ENG_Std:Token_Main_Lemma_Std +
    (1 | Group_No) + (1 | Item),
  data = FCT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)


## Mood Production, nested
Mood_Production_Frequency = glmer(
  Average ~ FofA_Std:Token_Main_Lemma_Std + FofA_Std + Token_Main_Lemma_Std + DELE_Std +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Mood_Production_NoDELE = glmer(
  Average ~ FofA_Std:Token_Main_Lemma_Std + FofA_Std + Token_Main_Lemma_Std +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Mood_Production_NoToken = glmer(
  Average ~ FofA_Std:Token_Main_Lemma_Std + FofA_Std +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Mood_Production_NoFofA = glmer(
  Average ~ FofA_Std:Token_Main_Lemma_Std +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Mood_Production_1 = glmer(
  Average ~ 1 +
    (1 | Group_No) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production_Frequency)
summary(Mood_Production_NoDELE)
summary(Mood_Production_NoToken)
summary(Mood_Production_NoFofA)
summary(Mood_Production_1)

anova(Mood_Production_Frequency, Mood_Production_NoDELE, test = "Chisq")
anova(Mood_Production_NoDELE, Mood_Production_NoToken, test = "Chisq")
anova(Mood_Production_NoToken, Mood_Production_NoFofA, test = "Chisq")
anova(Mood_Production_NoFofA, Mood_Production_1, test = "Chisq")