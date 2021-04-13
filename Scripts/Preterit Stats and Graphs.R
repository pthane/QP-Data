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
L2_Preterit_EPT = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv")
L2_Preterit_FCT = read_csv("./CSV Files/L2 Learners/L2 Learners FCT Preterit Data.csv")
Heritage_Preterit_EPT = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
Heritage_Preterit_FCT = read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
Comparison_Preterit_EPT = read.csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
Comparison_Preterit_FCT = read.csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv")


## Create combined datasets
Preterit_EPT = rbind(L2_Preterit_EPT, Heritage_Preterit_EPT, Comparison_Preterit_EPT)
Preterit_FCT = rbind(L2_Preterit_FCT, Heritage_Preterit_FCT, Comparison_Preterit_FCT)
Preterit_Aggregate = rbind(L2_Preterit_EPT, L2_Preterit_FCT)


## Generate heritage averages
Heritage_Preterit_EPT_Modified = aggregate(Heritage_Preterit_EPT$Response, list(Heritage_Preterit_EPT$Participant_ID), FUN = mean, na.rm = TRUE)
Heritage_Preterit_EPT_Modified = Heritage_Preterit_EPT_Modified %>% rename(Part_Avg = x)
Heritage_Preterit_EPT_Modified = left_join(Heritage_Preterit_EPT, Heritage_Preterit_EPT_Modified, by = c("Participant_ID" = "Group.1"))

Heritage_Preterit_FCT_Modified = aggregate(Heritage_Preterit_FCT$Response, list(Heritage_Preterit_FCT$Participant_ID), FUN = mean, na.rm = TRUE)
Heritage_Preterit_FCT_Modified = Heritage_Preterit_FCT_Modified %>% rename(Part_Avg = x)
Heritage_Preterit_FCT_Modified = left_join(Heritage_Preterit_FCT, Heritage_Preterit_FCT_Modified, by = c("Participant_ID" = "Group.1"))

L2_Preterit_EPT_Modified = aggregate(L2_Preterit_EPT$Response, list(L2_Preterit_EPT$Participant_ID), FUN = mean, na.rm = TRUE)
L2_Preterit_EPT_Modified = L2_Preterit_EPT_Modified %>% rename(Part_Avg = x)
L2_Preterit_EPT_Modified = left_join(L2_Preterit_EPT, L2_Preterit_EPT_Modified, by = c("Participant_ID" = "Group.1"))

L2_Preterit_FCT_Modified = aggregate(L2_Preterit_FCT$Response, list(L2_Preterit_FCT$Participant_ID), FUN = mean, na.rm = TRUE)
L2_Preterit_FCT_Modified = L2_Preterit_FCT_Modified %>% rename(Part_Avg = x)
L2_Preterit_FCT_Modified = left_join(L2_Preterit_FCT, L2_Preterit_FCT_Modified, by = c("Participant_ID" = "Group.1"))

Preterit_EPT_Aggregate = aggregate(Preterit_EPT$Response, list(Preterit_EPT$Participant_ID), FUN = mean, na.rm = TRUE)
Preterit_EPT_Aggregate = Preterit_EPT_Aggregate %>% rename(Part_Avg = x)
Preterit_EPT_Aggregate = left_join(Preterit_EPT, Preterit_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Preterit_FCT_Aggregate = aggregate(Preterit_FCT$Response, list(Preterit_FCT$Participant_ID), FUN = mean, na.rm = TRUE)
Preterit_FCT_Aggregate = Preterit_FCT_Aggregate %>% rename(Part_Avg = x)
Preterit_FCT_Aggregate = left_join(Preterit_FCT, Preterit_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


## Create additional aggregated datasets
Heritage_Preterit_Aggregate = rbind(Heritage_Preterit_EPT_Modified, Heritage_Preterit_FCT_Modified)
L2_Preterit_Aggregate = rbind(L2_Preterit_EPT_Modified, L2_Preterit_FCT_Modified)
Master_Preterit_Aggregate = rbind(Preterit_EPT_Aggregate, Preterit_FCT_Aggregate)


# Plots
## Individual lexical items
Master_Preterit_Aggregate %>%
  ggplot(aes(x = MainVerb, y = Response, color = ExpGroup, shape = Task)) + 
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
Master_Preterit_Aggregate %>%
  ggplot(aes(x = Reg_Main, y = Response, color = ExpGroup, shape = Task)) + 
  facet_grid(cols = vars(ExpGroup)) +
  geom_hline(yintercept = 0.8, color = "white", size = 2) + 
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Morphological regularity", y = "Proportion of preterit responses", color = "Group", 
       title = "Morphological Regularity and Preterit Use") +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and proficiency
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "Correct responses on DELE proficiency measure", y = "Proportion of preterit responses", caption = "", 
       title = "Preterit by Proficiency, EAPT", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "Correct responses on DELE proficiency measure", y = "Proportion of preterit responses", caption = "", 
       title = "Preterit by Proficiency, APST", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))

Master_Preterit_Aggregate %>% 
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup, shape = Task)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "Correct responses on DELE proficiency measure", y = "Proportion of preterit responses", caption = "", 
       title = "Preterit by Proficiency, APST", color = "Group", shape = "Task") +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and use
Preterit_EPT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Proportion of preterit responses",
       title = "Preterit by Frequency of Use, EAPT", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))

Preterit_FCT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Proportion of preterit responses",
       title = "Preterit by Frequency of Use, APST", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))

Master_Preterit_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup, shape = Task)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Proportion of preterit responses",
       title = "Preterit by Frequency of Use", color = "Group", task = "Task") +
  theme(plot.title = element_text(hjust = 0.5))


# By age of acquisition of English
Heritage_Preterit_Aggregate %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg, color = Task)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  scale_x_continuous(breaks = seq(0, 7, 1),
                     limits = c(0, 7)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Age of onset of acquisition of English", y = "Proportion of preterit responses", caption = "", 
       title = "Preterit by Age of Acquisition of English") +
  theme(plot.title = element_text(hjust = 0.5))