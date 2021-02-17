# Packages
library(tidyverse)
library(lme4)
library(dplyr)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(kableExtra)
library(Hmisc)

# Preparation of data
## Load database
L2_EPT = read_csv("./CSV Files/L2 Learners/EPT Subjunctive Standardized L2 Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/FCT Subjunctive Standardized L2 Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/EPT Subjunctive Standardized Heritage Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/EPT Subjunctive Standardized Comparison Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/FCT Subjunctive Standardized Comparison Data.csv")


## Create combined datasets
Subjunctive_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
Subjunctive_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)
Subjunctive_Aggregate = rbind(Subjunctive_EPT, Subjunctive_FCT)


## Find participant averages
Heritage_EPT_Modified = aggregate(Heritage_EPT$Average, list(Heritage_EPT$Participant_ID), FUN = mean)
Heritage_EPT_Modified = Heritage_EPT_Modified %>% rename(Part_Avg = x)
Heritage_EPT_Modified = left_join(Heritage_EPT, Heritage_EPT_Modified, by = c("Participant_ID" = "Group.1"))

Heritage_FCT_Modified = aggregate(Heritage_FCT$Average, list(Heritage_FCT$Participant_ID), FUN = mean)
Heritage_FCT_Modified = Heritage_FCT_Modified %>% rename(Part_Avg = x)
Heritage_FCT_Modified = left_join(Heritage_FCT, Heritage_FCT_Modified, by = c("Participant_ID" = "Group.1"))

Subjunctive_EPT_Aggregate = aggregate(Subjunctive_EPT$Average, list(Subjunctive_EPT$Participant_ID), FUN = mean)
Subjunctive_EPT_Aggregate = Subjunctive_EPT_Aggregate %>% rename(Part_Avg = x)
Subjunctive_EPT_Aggregate = left_join(Subjunctive_EPT, Subjunctive_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Subjunctive_FCT_Aggregate = aggregate(Subjunctive_FCT$Average, list(Subjunctive_FCT$Participant_ID), FUN = mean)
Subjunctive_FCT_Aggregate = Subjunctive_FCT_Aggregate %>% rename(Part_Avg = x)
Subjunctive_FCT_Aggregate = left_join(Subjunctive_FCT, Subjunctive_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


# Plots
## Individual lexical items
Subjunctive_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup, shape = Task)) + 
  geom_hline(yintercept = 0.8, color = 'white', size = 2) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Matrix verb", y = "Proportion of subjunctive responses", caption = "", 
       title = "Average Use of Subjunctive by Matrix Item") +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and proficiency
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of subjunctive responses', caption = '', 
       title = 'Subjunctive by Proficiency, EMPT') +
  theme(plot.title = element_text(hjust = 0.5))

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of subjunctive responses', caption = '', 
       title = 'Subjunctive by Proficiency, MSPT') +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and use
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of subjunctive responses',
       title = 'Subjunctive by Frequency of Use, EMPT') +
  theme(plot.title = element_text(hjust = 0.5))

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of subjunctive responses',
       title = 'Subjunctive by Frequency of Use, MSPT') +
  theme(plot.title = element_text(hjust = 0.5))


## By age of acquisition
Heritage_EPT_Modified %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  ylim(c(0,1)) +
  labs(x = 'Age of acquisition of English', y = 'Proportion of subjunctive responses', caption = '', 
       title = 'Subjunctive by Age of Acquisition, EMPT') +
  theme(plot.title = element_text(hjust = 0.5))

Heritage_FCT_Modified %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  ylim(c(0,1)) +
  labs(x = 'Age of acquisition of English', y = 'Proportion of subjunctive responses', caption = '', 
       title = 'Subjunctive by Age of Acquisition, MSPT') +
  theme(plot.title = element_text(hjust = 0.5))
