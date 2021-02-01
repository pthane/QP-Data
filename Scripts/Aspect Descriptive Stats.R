# Packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(kableExtra)

#Preparation of data
## Load database
L2_EPT = read_csv("./CSV Files/L2 Learners/EPT Preterit Standardized L2 Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/FCT Preterit Standardized L2 Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/EPT Preterit Standardized Heritage Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/FCT Preterit Standardized Heritage Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/EPT Preterit Standardized Comparison Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/FCT Preterit Standardized Comparison Data.csv")


# Descriptive stats for aspect
## Descriptive stats by experimental group
EPT_Aspect_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

FCT_Aspect_Data %>%
  group_by(ExpGroup) %>%
  summarize(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

# Descriptive stats by heritage group
EPT_Aspect_Data %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))

FCT_Aspect_Data %>%
  group_by(HerGroup) %>%
  summarise(Average = mean(Average, na.rm = T), SD = sd(Average, na.rm = T))


# Aspect plots
EPT_Aspect_Data %>%
  ggplot(aes(x = Condition, y = Average, color = ExpGroup)) + 
  facet_grid(. ~ ExpGroup) + 
  geom_hline(yinterEPT = 0.5, color = 'white', size = 2) + 
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
  geom_hline(yinterEPT = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean,
               geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'Condition', y = 'Proportion of expected responses', caption = '', 
       title = 'Descriptive Statistics by Preterit Condition for Comprehension')