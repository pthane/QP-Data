# Prepare environment
library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)
library(broom)
library(broomExtra)

options(scipen = 999)


# Load dataframes
EPT_Aspect_Data <- read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
EPT_Mood_Data <- read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
FCT_Aspect_Data <- read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
FCT_Mood_Data <- read_csv("./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
Composite_Aspect_Data <- read_csv("./CSV Files/Heritage/Heritage Aggregate Preterit Data.csv")
Composite_Mood_Data <- read_csv("./CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")


## Bind data
rbind(EPT_Aspect_Data, EPT_Mood_Data)

# Prepare models
## Prepare production model
Aspect_Production <- glmer(
  Response ~ FofA_Std + AoA_ENG_Std + DELE_Std + FofA_Std:AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Aspect_Data,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)


## Prepare comprehension model
Aspect_Comprehension = glmer(
  Response ~ FofA_Std + AoA_ENG_Std + DELE_Std + FofA_Std:AoA_ENG_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Aspect_Data,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)


# Generate nested models
## NNMC for production
Prod_Null <- glmer(Response ~ 1 +
                     (1 | Participant_ID) + (1 | Item),
                   data = EPT_Aspect_Data,
                   family = "binomial")

Prod_FofA <- glmer(Response ~ 1 + FofA_Std +
                     (1 | Participant_ID) + (1 | Item),
                   data = EPT_Aspect_Data,
                   family = "binomial")

Prod_AoA_ENG <- glmer(Response ~ 1 + FofA_Std + AoA_ENG_Std +
                     (1 | Participant_ID) + (1 | Item),
                   data = EPT_Aspect_Data,
                   family = "binomial")

Prod_DELE <- glmer(Response ~ 1 + FofA_Std + AoA_ENG_Std + DELE_Std +
                        (1 | Participant_ID) + (1 | Item),
                      data = EPT_Aspect_Data,
                      family = "binomial")

Prod_Interaction <- glmer(Response ~ 1 + FofA_Std + AoA_ENG_Std + FofA_Std:AoA_ENG_Std +
                        (1 | Participant_ID) + (1 | Item),
                      data = EPT_Aspect_Data,
                      family = "binomial")

anova(Prod_Null, Prod_FofA, Prod_AoA_ENG, Prod_DELE, Prod_Interaction, test = "Chisq")
