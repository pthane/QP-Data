library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 999)

## Load CSV files
Heritage_EPT = read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
Heritage_Aggregate = read_csv("./CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")
L2_EPT = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Subjunctive Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/L2 Learners FCT Subjunctive Data.csv")
L2_Aggregate = read_csv("./CSV Files/L2 Learners/L2 Learners Aggregate Subjunctive Data.csv")


# Heritage omnibus
HS_Categorical_Omnibus = glmer(
  ExpLI ~ Task + ProfGroup + FofA_Std + MV_Class + SV_Class + ProfGroup:SV_Class + ProfGroup:MV_Class + FofA_Std:SV_Class + FofA_Std:MV_Class +
    (1 | Participant_ID) + (1 | Item),
  data = Heritage_Aggregate, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(HS_Categorical_Omnibus)


# Heritage production
HS_Categorical_Production = glmer(
  ExpLI ~ ProfGroup + FofA_Std + SV_Class + MV_Class + ProfGroup:SV_Class + ProfGroup:MV_Class + FofA_Std:SV_Class + FofA_Std:MV_Class +
    (1 | Participant_ID) + (1 | Item),
  data = Heritage_EPT, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(HS_Categorical_Production)


# Heritage comprehension
HS_Categorical_Comprehension = glmer(
  ExpLI ~ ProfGroup + FofA_Std + SV_Class + MV_Class + ProfGroup:SV_Class + ProfGroup:MV_Class + FofA_Std:SV_Class + FofA_Std:MV_Class +
    (1 | Participant_ID) + (1 | Item),
  data = Heritage_FCT, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(HS_Categorical_Comprehension)


# L2 omnibus
L2_Categorical_Omnibus = glmer(
  ExpLI ~ Task + ProfGroup + FofA_Std + SV_Class + MV_Class + ProfGroup:SV_Class + ProfGroup:MV_Class + FofA_Std:SV_Class + FofA_Std:MV_Class +
    (1 | Participant_ID) + (1 | Item),
  data = L2_Aggregate, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(L2_Categorical_Omnibus)


# L2 production
L2_Categorical_Production = glmer(
  ExpLI ~ ProfGroup + FofA_Std + SV_Class + MV_Class + ProfGroup:SV_Class + ProfGroup:MV_Class + FofA_Std:SV_Class + FofA_Std:MV_Class +
    (1 | Participant_ID) + (1 | Item),
  data = L2_EPT, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(L2_Categorical_Production)


# L2 comprehension
L2_Categorical_Comprehension = glmer(
  ExpLI ~ ProfGroup + FofA_Std + SV_Class + MV_Class + ProfGroup:SV_Class + ProfGroup:MV_Class + FofA_Std:SV_Class + FofA_Std:MV_Class +
    (1 | Participant_ID) + (1 | Item),
  data = L2_FCT, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(L2_Categorical_Comprehension)
