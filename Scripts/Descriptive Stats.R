# Packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)

#Preparation of data
## Load database
CEPT_Aspect_Data = read_csv("./CSV Files/CEPT Preterit Raw Data.csv")
CEPT_Aspect_Standardized = read_csv("./CSV Files/CEPT Preterit Standardized Data.csv")
CEPT_Mood_Data = read_csv("./CSV Files/CEPT Subjunctive Raw Data.csv")
CEPT_Mood_Standardized = read_csv("./CSV Files/CEPT Subjunctive Standardized Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/FCT Preterit Raw Data.csv")
FCT_Aspect_Standardized = read_csv("./CSV Files/FCT Preterit Standardized Data.csv")
FCT_Mood_Data = read_csv("./CSV Files/FCT Subjunctive Raw Data.csv")
FCT_Mood_Standardized = read_csv("./CSV Files/FCT Subjunctive Raw Data.csv")


# Descriptive stats for aspect
## Descriptive stats by experimental group
CEPT_Aspect_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

FCT_Aspect_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

# Descriptive stats by heritage group
CEPT_Aspect_Data %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

FCT_Aspect_Data %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))


#Descriptive stats for aspect
## Descriptive stats by experimental group
CEPT_Mood_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

FCT_Mood_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

# Descriptive stats by heritage group
CEPT_Mood_Data %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

FCT_Mood_Data %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))


# Aspect plots
CEPT_Aspect_Data %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Preterit Condition for Production')

FCT_Aspect_Data %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Preterit Condition for Comprehension')


## Mood plots
CEPT_Mood_Data %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Subjunctive Condition for Production')

FCT_Mood_Data %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Subjunctive Condition for Comprehension')


# Functions of Lexical Frequency and Activation
CEPT_Mood_Data %>%
  ggplot(aes(x = LF_Main, y = Average, color = ExpGroup)) +
  xlim(0,10000000) +
  ylim(0,1)