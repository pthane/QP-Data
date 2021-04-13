# Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(TOSTER)

options(scipen = 99)

# Preparation of data
Heritage_Participant_Data = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
L2_Participant_Data = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv")
Comparison_Participant_Data = read_csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")

Participant_Data = rbind(Heritage_Participant_Data, L2_Participant_Data, Comparison_Participant_Data)
Participant_Data_Singular_Item = Participant_Data %>% 
  filter(Seq_Item == "2")

# Correlation measure for HS use/proficiency
Heritage_Participant_Data %>% 
  ggplot(aes(x = DELE, y = FofA, color = AoA_ENG)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  scale_y_continuous(breaks = seq (0, 50, 10),
                     limits = c(0, 50)) +
  labs(x = 'Proficiency score (DELE)', y = 'Self-reported frequency of use', caption = '', 
       title = 'Heritage Bilingual Proficiency and Patterns of Language Use', color = "Age of acq.") +
  theme(plot.title = element_text(hjust = 0.5))

Heritage_Proficiency_FofU_Correlation <- lm(FofA_Std ~ DELE_Std, data = Heritage_Participant_Data)
summary(Heritage_Proficiency_FofU_Correlation)

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


## Frequency of use
FofA = Participant_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(FofA), na.rm = T, SD = sd(FofA, na.rm = T), max(FofA), min(FofA))


# Graph proficiency groups
Participant_Data_Singular_Item %>% 
  ggplot(aes(x = DELE, fill = ExpGroup)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_continuous(breaks = seq (25, 50, 1),
                     limits = c(24, 51)) +
  scale_y_continuous(breaks = seq(0, 6, 2),
                     limits = c(0, 6)) +
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "DELE Proficiency Score", y = "Number of participants within range", title = "Participants by Group and Proficiency Score", fill = "Group") +
  theme(plot.title = element_text(hjust = 0.5))
