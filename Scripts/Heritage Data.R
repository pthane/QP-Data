# Run packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(ggeffects)
library(emmeans)
library(effects)
library(sjmisc)
library(sjPlot)

#Preparation of data
## Load database
CEPT_Aspect_Data = read_csv("./CSV Files/Heritage/CEPT Preterit Standardized Heritage Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/Heritage/FCT Preterit Standardized Heritage Data.csv")
Composite_Aspect_Data = read_csv("./CSV Files/Heritage/Composite Preterit Standardized Heritage Data.csv")


# Aspect GLMMs
## Aspect production
Aspect_Production = lmer(
  Average ~ FofA_Std + LF_Std + TF_Main + DELE_Std + FofA_Std:LF_Std + FofA_Std:TF_Main + DELE_Std:LF_Std:TF_Main + FofA_Std:LF_Std:TF_Main +
    (1 | Participant) + (1 | Item),
  data = CEPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)

## Aspect comprehension
Aspect_Comprehension = lmer(
  Average ~ FofA_Std + LF_Std + TF_Main + DELE_Std + FofA_Std:LF_Std + FofA_Std:TF_Main + DELE_Std:LF_Std:TF_Main + FofA_Std:LF_Std:TF_Main +
    (1 | Participant) + (1 | Item),
  data = CEPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Comprehension)
print(Aspect_Comprehension, correlation = TRUE)


## Aspect composite
Aspect_Composite = lmer(
  Average ~ FofA_Std + LF_Std + TF_Main + DELE_Std + FofA_Std:LF_Std + FofA_Std:TF_Main + DELE_Std:LF_Std:TF_Main + FofA_Std:LF_Std:TF_Main +
    (1 | Participant) + (1 | Item),
  data = CEPT_Aspect_Data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Composite)
print(Aspect_Composite, correlation = TRUE)