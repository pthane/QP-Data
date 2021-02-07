# Packages
library(tidyverse)
library(lme4)
library(dplyr)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(kableExtra)

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
Subjunctive_EPT_Aggregate = aggregate(Subjunctive_EPT$Average, list(Subjunctive_EPT$Participant_ID), FUN = mean)
Subjunctive_EPT_Aggregate = Subjunctive_EPT_Aggregate %>% rename(Part_Avg = x)
Subjunctive_EPT_Aggregate = left_join(Subjunctive_EPT, Subjunctive_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Subjunctive_FCT_Aggregate = aggregate(Subjunctive_FCT$Average, list(Subjunctive_FCT$Participant_ID), FUN = mean)
Subjunctive_FCT_Aggregate = Subjunctive_FCT_Aggregate %>% rename(Part_Avg = x)
Subjunctive_FCT_Aggregate = left_join(Subjunctive_FCT, Subjunctive_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


# Descriptive stats
## Descriptive stats by experimental group
Subjunctive_EPT_Aggregate %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

Subjunctive_FCT_Aggregate %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))


# Plots
## Individual lexical items
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive by matrix item, EPT')

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive by matrix item, FCT')

Subjunctive_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup, shape = Task)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive by matrix item')


## By accuracy across morphological regularity
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = Type_Main, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Regularity of matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive by morphological regularity, EPT')

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = Type_Main, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Regularity of matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive by morphological regularity, FCT')

Subjunctive_Aggregate %>%
  ggplot(aes(x = Type_Main, y = Average, color = ExpGroup, shape = Task)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Frequency of matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive by morphological regularity')


## By accuracy across condition
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Frequency of matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive, EPT')

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Frequency of matrix verb', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Average use of Subjunctive, FCT')


## By average accuracy and proficiency
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Use of Subjunctive by proficiency, EPT')

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Use of Subjunctive by proficiency, FCT')


## By average accuracy and activation
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(0,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Use of Subjunctive by frequency of use, EPT')

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(0,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of Subjunctive responses', caption = '', 
       title = 'Use of Subjunctive by frequency of use, FCT')

# scale_y_continuous(breaks = seq(0, 50, 5)) +