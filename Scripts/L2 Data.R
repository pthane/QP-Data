# Run data
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(ggeffects)
library(emmeans)

options(scipen = 99)

#Preparation of data
## Load database
CEPT_Aspect_Data = read_csv("./CSV Files/L2 Learners/CEPT Preterit Standardized L2 Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/L2 Learners/FCT Preterit Standardized L2 Data.csv")
Composite_Aspect_Data = read_csv("./CSV Files/L2 Learners/Composite Preterit Standardized L2 Data.csv")
CEPT_Mood_Data = read_csv("./CSV Files/L2 Learners/CEPT Subjunctive Standardized L2 Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/L2 Learners/FCT Subjunctive Standardized L2 Data.csv")
Composite_Mood_Data = read_csv("./CSV Files/L2 Learners/Composite Subjunctive Standardized L2 Data.csv")


# Aspect GLMMs
## Aspect production
Aspect_Production = lmer(
  Average ~ ExpGroup + FofA_Std + LF_Std + DELE_Std + ExpGroup:FofA_Std + ExpGroup:LF_Std + ExpGroup:DELE_Std:LF_Std + ExpGroup:FofA_Std:LF_Std +
    (1 | Participant) + (1 | Item),
  data = CEPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)
print(Aspect_Production, correlation = TRUE)


## Aspect comprehension
Aspect_Comprehension = lmer(
  Average ~ ExpGroup + FofA_Std + LF_Std + DELE_Std + ExpGroup:FofA_Std + ExpGroup:LF_Std + ExpGroup:DELE_Std:LF_Std + ExpGroup:FofA_Std:LF_Std +
    (1 | Participant) + (1 | Item),
  data = FCT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)
print(Aspect_Comprehension, correlation = TRUE)


## Aspect composite
Aspect_Composite = lmer(
  Average ~ ExpGroup + FofA_Std + LF_Std + DELE_Std + ExpGroup:FofA_Std + ExpGroup:LF_Std + ExpGroup:DELE_Std:LF_Std + ExpGroup:FofA_Std:LF_Std + DELE_Std:FofA_Std:LF_Std +
    (1 | Participant) + (1 | Item),
  data = Composite_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Composite)
print(Aspect_Composite, correlation = TRUE)


# Mood GLMMs
## Mood production
Mood_Production = lmer(
  Average ~ ExpGroup + FofA_Std + LF_Main_Std + DELE_Std + ExpGroup:FofA_Std + ExpGroup:LF_Main_Std + ExpGroup:DELE_Std:LF_Main_Std + ExpGroup:FofA_Std:LF_Main_Std +
    (1 | Participant) + (1 | Item),
  data = CEPT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)
print(Mood_Production, correlation = TRUE)


## Mood comprehension
Mood_Comprehension = lmer(
  Average ~ ExpGroup + FofA_Std + LF_Main_Std + DELE_Std + ExpGroup:FofA_Std + ExpGroup:LF_Main_Std + ExpGroup:DELE_Std:LF_Main_Std + ExpGroup:FofA_Std:LF_Main_Std +
    (1 | Participant) + (1 | Item),
  data = FCT_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)
print(Mood_Comprehension, correlation = TRUE)


## Mood composite
Mood_Composite = lmer(
  Average ~ ExpGroup + FofA_Std + LF_Main_Std + DELE_Std + ExpGroup:FofA_Std + ExpGroup:LF_Main_Std + ExpGroup:DELE_Std:LF_Main_Std + ExpGroup:FofA_Std:LF_Main_Std +
    (1 | Participant) + (1 | Item),
  data = Composite_Mood_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Composite)
print(Mood_Composite, correlation = TRUE)