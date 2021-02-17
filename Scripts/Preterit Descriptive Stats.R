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
L2_EPT = read_csv("./CSV Files/L2 Learners/EPT Preterit Standardized L2 Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/FCT Preterit Standardized L2 Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/EPT Preterit Standardized Heritage Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/FCT Preterit Standardized Heritage Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/EPT Preterit Standardized Comparison Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/FCT Preterit Standardized Comparison Data.csv")


## Create combined datasets
Preterit_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
Preterit_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)
Preterit_Aggregate = rbind(Preterit_EPT, Preterit_FCT)


## Find participant averages
Heritage_EPT_Modified = aggregate(Heritage_EPT$Average, list(Heritage_EPT$Participant_ID), FUN = mean)
Heritage_EPT_Modified = Heritage_EPT_Modified %>% rename(Part_Avg = x)
Heritage_EPT_Modified = left_join(Heritage_EPT, Heritage_EPT_Modified, by = c("Participant_ID" = "Group.1"))

Heritage_FCT_Modified = aggregate(Heritage_FCT$Average, list(Heritage_FCT$Participant_ID), FUN = mean)
Heritage_FCT_Modified = Heritage_FCT_Modified %>% rename(Part_Avg = x)
Heritage_FCT_Modified = left_join(Heritage_FCT, Heritage_FCT_Modified, by = c("Participant_ID" = "Group.1"))

Preterit_EPT_Aggregate = aggregate(Preterit_EPT$Average, list(Preterit_EPT$Participant_ID), FUN = mean)
Preterit_EPT_Aggregate = Preterit_EPT_Aggregate %>% rename(Part_Avg = x)
Preterit_EPT_Aggregate = left_join(Preterit_EPT, Preterit_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Preterit_FCT_Aggregate = aggregate(Preterit_FCT$Average, list(Preterit_FCT$Participant_ID), FUN = mean)
Preterit_FCT_Aggregate = Preterit_FCT_Aggregate %>% rename(Part_Avg = x)
Preterit_FCT_Aggregate = left_join(Preterit_FCT, Preterit_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


# Group-wise models
Preterit_Task_Group_Effects = lmer(
  Average ~ ExpGroup + Task + ExpGroup:Task +
    (1 | Group_No) + (1 | Item),
  data = Preterit_Aggregate,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Preterit_Task_Group_Effects)


# Plots
## Individual lexical items
Preterit_Aggregate %>%
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


## By morphological regularity and frequency
Preterit_Aggregate %>%
  ggplot(aes(x = Reg_Main, y = Average, color = ExpGroup, shape = Task)) + 
  facet_grid(cols = vars(ExpGroup)) +
  geom_hline(yintercept = 0.8, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_se,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = 'Log-transformed lemma frequency in Davies (2006)', y = 'Proportion of preterit responses', caption = '', 
       title = 'Morphological Regularity, Lexical Frequency, and Preterit Use') +
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
