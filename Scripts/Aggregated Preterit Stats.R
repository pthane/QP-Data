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
L2_EPT = read_csv("./CSV Files/L2 Learners/EPT Preterit Standardized L2 Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/FCT Preterit Standardized L2 Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/EPT Preterit Standardized Heritage Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/FCT Preterit Standardized Heritage Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/EPT Preterit Standardized Comparison Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/FCT Preterit Standardized Comparison Data.csv")

## Merge databases
L2_Aggregate = rbind(L2_EPT, L2_FCT)
Heritage_Aggregate = rbind(Heritage_EPT, Heritage_FCT)
Comparison_Aggregate = rbind(Comparison_EPT, Comparison_FCT)
ExpGroup_Aggregate = rbind(L2_EPT, L2_FCT, Heritage_EPT, Heritage_FCT)
AllGroup_Aggregate = rbind(L2_EPT, L2_FCT, Heritage_EPT, Heritage_FCT, Comparison_EPT, Comparison_FCT)
AllGroup_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
AllGroup_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)

# Aspect models
Aspect_Aggregate = lmer(
  Average ~ HerGroup + FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Type_Main + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Participant),
  data = AllGroup_EPT,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

Aspect_Heritage = lmer(
  Average ~ Task + FofA_Std + Token_Lemma_Std + Type_Main + DELE_Std + Task:DELE_Std + Task:FofA_Std + Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std + FofA_Std:Token_Lemma_Std + DELE_Std:Token_Lemma_Std:Type_Main + FofA_Std:Token_Lemma_Std:Type_Main +
    (1 | Participant) + (1 | Item),
  data = Heritage_Aggregate,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Aggregate)
summary(Aspect_Heritage)