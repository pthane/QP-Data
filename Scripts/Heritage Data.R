# Run packages
library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)
library(broom)
library(broomExtra)

options(scipen = 999)

# Preparation of data
## Load database
EPT_Aspect_Data = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
EPT_Mood_Data = read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
Aspect_Composite = read_csv("./CSV Files/Heritage/Heritage Aggregate Preterit Data.csv")
Mood_Composite = read_csv("./CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")


# Aspect GLMMs
## Omnibus model
Aspect_Omnibus = glmer(
  Response ~ Task + Token_Main_Std * Reg_Main * FofA_Std + AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = Aspect_Composite, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Omnibus)
broom::tidy(Aspect_Omnibus, conf.int = TRUE)


## Aspect production
Aspect_Production = glmer(
  Response ~ Token_Main_Std * Reg_Main * FofA_Std + AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)
broom::tidy(Aspect_Production, conf.int = TRUE)


## Aspect comprehension
Aspect_Comprehension = glmer(
  Response ~ Token_Main_Std * Reg_Main * FofA_Std + AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)
broom::tidy(Aspect_Comprehension, conf.int = TRUE)


# Mood GLMMs
## Omnibus model
Mood_Omnibus = glmer(
  Response ~ Task + DELE_Std + FofA_Std + Token_Sub_Std + Token_Main_Std + DELE_Std:Token_Sub_Std + DELE_Std:Token_Main_Std + FofA_Std:Token_Sub_Std + FofA_Std:Token_Main_Std +
    (1 | Participant_ID) + (1 | Item),
  data = Mood_Composite, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Omnibus)
broom::tidy(Mood_Omnibus, conf.int = TRUE)


## Mood production
### Mood production with all variables
Mood_Production = glmer(
  Response ~ DELE_Std + FofA_Std + Token_Sub_Std + Token_Main_Std + DELE_Std:Token_Sub_Std + DELE_Std:Token_Main_Std + FofA_Std:Token_Sub_Std + FofA_Std:Token_Main_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)
broom::tidy(Mood_Production, conf.int = TRUE)

## Mood comprehension
### Mood comprehension with all variables
Mood_Comprehension = glmer(
  Response ~ DELE_Std + FofA_Std + Token_Sub_Std + Token_Main_Std + DELE_Std:Token_Sub_Std + DELE_Std:Token_Main_Std + FofA_Std:Token_Sub_Std + FofA_Std:Token_Main_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Mood_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)
broom::tidy(Mood_Comprehension, conf.int = TRUE)
