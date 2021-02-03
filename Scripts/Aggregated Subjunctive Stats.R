# Packages
library(tidyverse)
library(lme4)
library(dplyr)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(kableExtra)

options(scipen = 999)

# Preparation of data
## Load database
L2_EPT = read_csv("./CSV Files/L2 Learners/EPT Subjunctive Standardized L2 Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/FCT Subjunctive Standardized L2 Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/EPT Subjunctive Standardized Heritage Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/EPT Subjunctive Standardized Comparison Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/FCT Subjunctive Standardized Comparison Data.csv")

## Merge databases
L2_Aggregate = rbind(L2_EPT, L2_FCT)
Heritage_Aggregate = rbind(Heritage_EPT, Heritage_FCT)
Comparison_Aggregate = rbind(Comparison_EPT, Comparison_FCT)
ExpGroup_Aggregate = rbind(L2_EPT, L2_FCT, Heritage_EPT, Heritage_FCT)
AllGroup_Aggregate = rbind(L2_EPT, L2_FCT, Heritage_EPT, Heritage_FCT, Comparison_EPT, Comparison_FCT)
AllGroup_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
AllGroup_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)

# Mood models
Mood_Aggregate_HerGroup = lmer(
  Average ~ HerGroup + Task + FofA_Std + DELE_Std + Token_Main_Lemma_Std + HerGroup:FofA_Std + HerGroup:DELE_Std + FofA_Std:Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + Task:FofA_Std:Token_Main_Lemma_Std + HerGroup:FofA_Std:Token_Main_Lemma_Std +
    (1 | Participant),
  data = ExpGroup_Aggregate,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Mood_Aggregate_ExpGroup = lmer(
  Average ~ ExpGroup + Task + FofA_Std + DELE_Std + Token_Main_Lemma_Std + Task:ExpGroup + Task:DELE_Std + Task:FofA_Std + ExpGroup:FofA_Std + ExpGroup:DELE_Std + FofA_Std:Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + Task:FofA_Std:Token_Main_Lemma_Std + ExpGroup:FofA_Std:Token_Main_Lemma_Std +
    (1 | Participant),
  data = ExpGroup_Aggregate,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Mood_Heritage = lmer(
  Average ~ Task + HerGroup + FofA_Std + DELE_Std + Token_Main_Lemma_Std + Task:FofA_Std + Task:DELE_Std + FofA_Std:Token_Main_Lemma_Std + DELE_Std:Token_Main_Lemma_Std + HerGroup:Token_Main_Lemma_Std + HerGroup:FofA_Std:Token_Main_Lemma_Std + HerGroup:DELE_Std:Token_Main_Lemma_Std +
    (1 | Participant) + (1 | Item),
  data = Heritage_Aggregate
)

summary(Mood_Aggregate_HerGroup)
summary(Mood_Aggregate_ExpGroup)
summary(Mood_Heritage)
