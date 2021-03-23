library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)

# Preparation of data
Heritage_Participant_Data = read_csv("./CSV Files/Heritage/EPT Subjunctive Standardized Heritage Data.csv")
L2_Participant_Data = read_csv("./CSV Files/L2 Learners/EPT Subjunctive Standardized L2 Data.csv")
Comparison_Participant_Data = read_csv("./CSV Files/Comparison/EPT Subjunctive Standardized Comparison Data.csv")

Participant_Data = rbind(Heritage_Participant_Data, L2_Participant_Data, Comparison_Participant_Data)


# Correlation measure for HS use/proficiency
Heritage_Participant_Data %>% 
  ggplot(aes(x = DELE, y = FofA, color = AoA_ENG)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = 'Proficiency score (DELE)', y = 'Self-reported frequency of use', caption = '', 
       title = 'Proficiency and Patterns of Language Use')


# Participant statistics
## Age
Age = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Age), na.rm = T, SD = sd(Age, na.rm = T), max(Age), min(Age))


## Age of acquisition of Spanish
Spanish_Acquisition = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(AoA_SPA), na.rm = T, SD = sd(AoA_SPA, na.rm = T), max(AoA_SPA), min(AoA_SPA))


## Age of acquisition of English
English_Acquisition = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(AoA_ENG), na.rm = T, SD = sd(AoA_ENG, na.rm = T), max(AoA_ENG), min(AoA_ENG))


## Length of residence
LOR = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(LOR), na.rm = T, SD = sd(LOR, na.rm = T), max(LOR), min(LOR))


## Self-rated proficiency
SelfProf = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(SelfProf), na.rm = T, SD = sd(SelfProf, na.rm = T), max(SelfProf), min(SelfProf))


## Self-rated dominance
SelfDom = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(SelfDom), na.rm = T, SD = sd(SelfDom, na.rm = T), max(SelfDom), min(SelfDom))


## DELE scores
DELE = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(DELE), na.rm = T, SD = sd(DELE, na.rm = T), max(DELE), min(DELE))

## Frequency of activation
FofA = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(FofA), na.rm = T, SD = sd(FofA, na.rm = T), max(FofA), min(FofA))
