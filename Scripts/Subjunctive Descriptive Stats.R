# Packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(kableExtra)

#Preparation of data
## Load database
L2_EPT = read_csv("./CSV Files/L2 Learners/EPT Subjunctive Standardized L2 Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/FCT Subjunctive Standardized L2 Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/EPT Subjunctive Standardized Heritage Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/FCT Subjunctive Standardized Heritage Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/EPT Subjunctive Standardized Comparison Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/FCT Subjunctive Standardized Comparison Data.csv")


## Create combined dataset
Subjunctive_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
Subjunctive_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)

# Descriptive stats
## Descriptive stats by experimental group
Subjunctive_EPT %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

Subjunctive_EPT %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

# Descriptive stats by heritage group
Subjunctive_EPT %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

Subjunctive_EPT %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))


# Plots by accuracy across condition
Subjunctive_EPT %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Condition, Production Task')

Subjunctive_FCT %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Condition, Forced Choice Task')