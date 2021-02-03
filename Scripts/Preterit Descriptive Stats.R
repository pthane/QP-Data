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
Preterit_EPT_Aggregate = aggregate(Preterit_EPT$Average, list(Preterit_EPT$Participant_ID), FUN = mean)
Preterit_EPT_Aggregate = Preterit_EPT_Aggregate %>% rename(Part_Avg = x)
Preterit_EPT_Aggregate = left_join(Preterit_EPT, Preterit_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Preterit_FCT_Aggregate = aggregate(Preterit_FCT$Average, list(Preterit_FCT$Participant_ID), FUN = mean)
Preterit_FCT_Aggregate = Preterit_FCT_Aggregate %>% rename(Part_Avg = x)
Preterit_FCT_Aggregate = left_join(Preterit_FCT, Preterit_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


# Descriptive stats
## Descriptive stats by experimental group
Preterit_EPT_Aggregate %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

Preterit_FCT_Aggregate %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))


# Plots
## Individual lexical items
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of preterit by matrix item, EPT')

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of preterit by matrix item, FCT')

Preterit_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Average, color = ExpGroup, shape = Task)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of preterit by matrix item')


## By accuracy across morphological regularity
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = Type_Main, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Regularity of matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of preterit by morphological regularity, EPT')

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = Type_Main, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Regularity of matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of preterit by morphological regularity, FCT')

Preterit_Aggregate %>%
  ggplot(aes(x = Type_Main, y = Average, color = ExpGroup, shape = Task)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Frequency of matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of preterit by morphological regularity')


## By accuracy across condition
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Frequency of matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of Preterit, EPT')

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Frequency of matrix verb', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Average use of Preterit, FCT')


## By average accuracy and proficiency
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Use of preterit by proficiency, EPT')

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = 'Correct responses on DELE proficiency measure', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Use of Preterit by proficiency, FCT')


## By average accuracy and activation
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(0,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Use of preterit by frequency of use, EPT')

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(0,50)) +
  ylim(c(0,1)) +
  labs(x = 'Self-reported frequency of use of Spanish', y = 'Proportion of Preterit responses', caption = '', 
       title = 'Use of preterit by frequency of use, FCT')