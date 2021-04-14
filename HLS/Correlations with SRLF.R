library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(broom)
library(broomExtra)

options(scipen = 99)


# Load databases
EPT_Original <- read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
FCT_Original <- read_csv("./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
Composite_Original <- read_csv("./CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")
Matrix_Ratings <- read_csv("./CSV Files/Lexical Item Analysis/Matrix Verb List.csv")
Subordinate_Ratings <- read_csv("./CSV Files/Lexical Item Analysis/Subordinate Verb List.csv")


# Data preparation
## Prepare omnibus data (aggregate)
Composite_Mod <- merge(Composite_Original, Matrix_Ratings, by = "MainVerb", all.x = TRUE)

Composite_Final <- merge(Composite_Mod, Subordinate_Ratings, by = "SubVerb", all.x = TRUE) %>%
  mutate(Sub_SRLF_Std = (Sub_SRLF - mean(Sub_SRLF))/sd(Sub_SRLF),
         Matrix_SRLF_Std = (Matrix_SRLF - mean(Matrix_SRLF))/sd(Matrix_SRLF))


## Prepare production data
EPT_Mod <- merge(EPT_Original, Matrix_Ratings, by = "MainVerb", all.x = TRUE)

EPT_Final <- merge(EPT_Mod, Subordinate_Ratings, by = "SubVerb", all.x = TRUE) %>%
  mutate(Sub_SRLF_Std = (Sub_SRLF - mean(Sub_SRLF))/sd(Sub_SRLF),
         Matrix_SRLF_Std = (Matrix_SRLF - mean(Matrix_SRLF))/sd(Matrix_SRLF))


## Prepare interpretation data
FCT_Mod <- merge(FCT_Original, Matrix_Ratings, by = "MainVerb", all.x = TRUE)

FCT_Final <- merge(FCT_Mod, Subordinate_Ratings, by = "SubVerb", all.x = TRUE) %>%
  mutate(Sub_SRLF_Std = (Sub_SRLF - mean(Sub_SRLF))/sd(Sub_SRLF),
         Matrix_SRLF_Std = (Matrix_SRLF - mean(Matrix_SRLF))/sd(Matrix_SRLF))


# Models with SRLF
## Composite model
Mood_Omnibus = glmer(
  Response ~ Task + DELE_Std + Sub_SRLF_Std + Matrix_SRLF_Std + DELE_Std:Sub_SRLF_Std + DELE_Std:Matrix_SRLF_Std +
    (1 | Participant_ID) + (1 | Item),
  data = Composite_Final, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Omnibus)


## Production
Mood_Production = glmer(
  Response ~ DELE_Std + Sub_SRLF_Std + Matrix_SRLF_Std + DELE_Std:Sub_SRLF_Std + DELE_Std:Matrix_SRLF_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Final, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)


## Comprehension
Mood_Comprehension = glmer(
  Response ~ DELE_Std + Sub_SRLF_Std + Matrix_SRLF_Std + DELE_Std:Sub_SRLF_Std + DELE_Std:Matrix_SRLF_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Final, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Comprehension)