# Packages
library(tidyverse)
library(lme4)
library(dplyr)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(kableExtra)

options(sciphen = 999)

# Preparation of data
## Load database
L2_EPT = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/L2 Learners FCT Preterit Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv")


## Create combined datasets
Preterit_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
Preterit_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)
Preterit_Aggregate = rbind(Preterit_EPT, Preterit_FCT)


## Find participant averages
Heritage_EPT_Modified = aggregate(Heritage_EPT$ExpLI, list(Heritage_EPT$Participant_ID), FUN = mean)
Heritage_EPT_Modified = Heritage_EPT_Modified %>% rename(Part_Avg = x)
Heritage_EPT_Modified = left_join(Heritage_EPT, Heritage_EPT_Modified, by = c("Participant_ID" = "Group.1"))

Heritage_FCT_Modified = aggregate(Heritage_FCT$ExpLI, list(Heritage_FCT$Participant_ID), FUN = mean)
Heritage_FCT_Modified = Heritage_FCT_Modified %>% rename(Part_Avg = x)
Heritage_FCT_Modified = left_join(Heritage_FCT, Heritage_FCT_Modified, by = c("Participant_ID" = "Group.1"))

Preterit_EPT_Aggregate = aggregate(Preterit_EPT$ExpLI, list(Preterit_EPT$Participant_ID), FUN = mean)
Preterit_EPT_Aggregate = Preterit_EPT_Aggregate %>% rename(Part_Avg = x)
Preterit_EPT_Aggregate = left_join(Preterit_EPT, Preterit_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Preterit_FCT_Aggregate = aggregate(Preterit_FCT$ExpLI, list(Preterit_FCT$Participant_ID), FUN = mean)
Preterit_FCT_Aggregate = Preterit_FCT_Aggregate %>% rename(Part_Avg = x)
Preterit_FCT_Aggregate = left_join(Preterit_FCT, Preterit_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


# Plots
## Individual lexical items
Preterit_Aggregate %>%
  ggplot(aes(x = MainVerb, y = ExpForm, color = ExpGroup, shape = Task)) + 
  geom_hline(yintercept = 0.8, color = "white", size = 2) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  scale_x_discrete(labels = c("estar", "tener", "haber", "vivir", "gustar", "creer", "faltar", "amar", "doler", "excluir")) +
  labs(x = "State verb (from most to least frequent)", y = "Proportion of preterit responses", color = "Group", 
       title = "Average Use of Preterit by State Verb and Group") +
  theme(plot.title = element_text(hjust = 0.5))


## By morphological regularity and frequency
Preterit_Aggregate %>%
  ggplot(aes(x = Reg_Main, y = ExpLI, color = ExpGroup, shape = Task)) + 
  facet_grid(cols = vars(ExpGroup)) +
  geom_hline(yintercept = 0.8, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_se,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Morphological regularity", y = "Proportion of preterit responses", color = "Group", 
       title = 'Morphological Regularity and Preterit Use') +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and proficiency
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of preterit responses', caption = '', 
       title = 'Preterit by Proficiency, EAPT') +
  theme(plot.title = element_text(hjust = 0.5))

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of preterit responses', caption = '', 
       title = 'Preterit by Proficiency, ASPT') +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and use
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of preterit responses',
       title = 'Preterit by Frequency of Use, EAPT') +
  theme(plot.title = element_text(hjust = 0.5))

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of preterit responses',
       title = 'Preterit by Frequency of Use, ASPT') +
  theme(plot.title = element_text(hjust = 0.5))

# By age of acquisition of English
Heritage_EPT_Modified %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = 'Age of onset of acquisition of English', y = 'Proportion of subjunctive responses', caption = '', 
       title = 'Preterit by Age of Acquisition of English, EAPT') +
  theme(plot.title = element_text(hjust = 0.5))

Heritage_FCT_Modified %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = 'Age of onset of acquisition of English', y = 'Proportion of subjunctive responses', caption = '', 
       title = 'Preterit by Age of Acquisition of English, APST') +
  theme(plot.title = element_text(hjust = 0.5))
