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
Aspect_Composite = read_csv("./CSV Files/Comparison/Comparison Aggregate Preterit Data.csv")
Mood_Composite = read_csv("./CSV Files/Comparison/Comparison Aggregate Subjunctive Data.csv")


# Aspect GLMMs
## Omnibus model
Aspect_Omnibus = glmer(
  ExpForm ~ Task + Token_Main_Std * Reg_Main * FofA_Std + AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = Aspect_Composite, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Omnibus)


## Aspect production
Aspect_Production = glmer(
  ExpForm ~ Token_Main_Std * Reg_Main * FofA_Std + AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)


## Aspect comprehension
Aspect_Comprehension = glmer(
  ExpForm ~ Token_Main_Std * Reg_Main * FofA_Std + AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)


# Mood GLMMs
## Omnibus model
Mood_Omnibus = glmer(
  ExpLI ~ Task + DELE_Std + FofA_Std + Token_Sub_Std + Token_Main_Std + DELE_Std:Token_Sub_Std + DELE_Std:Token_Main_Std + FofA_Std:Token_Sub_Std + FofA_Std:Token_Main_Std +
    (1 | Participant_ID) + (1 | Item),
  data = Mood_Composite, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Omnibus)

## Mood production
### Mood production with all variables
Mood_Production = glmer(
  ExpLI ~ DELE_Std + FofA_Std + Token_Sub_Std + Token_Main_Std + DELE_Std:Token_Sub_Std + DELE_Std:Token_Main_Std + FofA_Std:Token_Sub_Std + FofA_Std:Token_Main_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)


## Mood comprehension
### Mood comprehension with all variables
Mood_Comprehension = glmer(
  ExpLI ~ DELE_Std + FofA_Std + Token_Sub_Std + Token_Main_Std + DELE_Std:Token_Sub_Std + DELE_Std:Token_Main_Std + FofA_Std:Token_Sub_Std + FofA_Std:Token_Main_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)