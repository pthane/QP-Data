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
L2_EPT = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Subjunctive Data.csv")
L2_FCT = read_csv("./CSV Files/L2 Learners/L2 Learners FCT Subjunctive Data.csv")
Heritage_EPT = read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
Heritage_FCT = read_csv("./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
Comparison_EPT = read.csv("./CSV Files/Comparison/Comparison EPT Subjunctive Data.csv")
Comparison_FCT = read.csv("./CSV Files/Comparison/Comparison FCT Subjunctive Data.csv")


## Create combined datasets
Subjunctive_EPT = rbind(L2_EPT, Heritage_EPT, Comparison_EPT)
Subjunctive_FCT = rbind(L2_FCT, Heritage_FCT, Comparison_FCT)
Subjunctive_Aggregate = rbind(Subjunctive_EPT, Subjunctive_FCT)


## Find participant averages
Heritage_EPT_Standardized = aggregate(Heritage_EPT$ExpLI, list(Heritage_EPT$Participant_ID), FUN = mean)
Heritage_EPT_Standardized = Heritage_EPT_Standardized %>% rename(Part_Avg = x)
Heritage_EPT_Standardized = left_join(Heritage_EPT, Heritage_EPT_Standardized, by = c("Participant_ID" = "Group.1"))

Heritage_FCT_Standardized = aggregate(Heritage_FCT$ExpLI, list(Heritage_FCT$Participant_ID), FUN = mean)
Heritage_FCT_Standardized = Heritage_FCT_Standardized %>% rename(Part_Avg = x)
Heritage_FCT_Standardized = left_join(Heritage_FCT, Heritage_FCT_Standardized, by = c("Participant_ID" = "Group.1"))

Subjunctive_EPT_Aggregate = aggregate(Subjunctive_EPT$ExpLI, list(Subjunctive_EPT$Participant_ID), FUN = mean)
Subjunctive_EPT_Aggregate = Subjunctive_EPT_Aggregate %>% rename(Part_Avg = x)
Subjunctive_EPT_Aggregate = left_join(Subjunctive_EPT, Subjunctive_EPT_Aggregate, by = c("Participant_ID" = "Group.1"))

Subjunctive_FCT_Aggregate = aggregate(Subjunctive_FCT$ExpLI, list(Subjunctive_FCT$Participant_ID), FUN = mean)
Subjunctive_FCT_Aggregate = Subjunctive_FCT_Aggregate %>% rename(Part_Avg = x)
Subjunctive_FCT_Aggregate = left_join(Subjunctive_FCT, Subjunctive_FCT_Aggregate, by = c("Participant_ID" = "Group.1"))


# Plots
## Individual matrix items
Subjunctive_Aggregate %>%
  ggplot(aes(x = MainVerb, y = ExpForm, color = ExpGroup, shape = Task)) + 
  geom_hline(yintercept = 0.8, color = "white", size = 2) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  scale_x_discrete(labels = c("esperar", "querer", "permitir", "necesitar", "pedir", "desear", "sugerir", "ordenar")) +
  labs(x = "Matrix verb (from highest to lowest frequency)", y = "Proportion of subjunctive responses", color = "Group",
       title = "Average Use of Subjunctive by Matrix Item") +
  theme(plot.title = element_text(hjust = 0.5))


## Individual subordinate items
Subjunctive_Aggregate %>%
  ggplot(aes(x = SubVerb, y = ExpForm, color = ExpGroup, shape = Task)) + 
  geom_hline(yintercept = 0.8, color = "white", size = 2) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  scale_x_discrete(labels = c("quedar", "enviar", "dejar", "tratar", "tomar", "viajar", "prestar", "bailar", "robar", "amar")) +
  labs(x = "Subordinate verb (from highest to lowest frequency)", y = "Proportion of subjunctive responses", color = "Group",
       title = "Average Use of Subjunctive by Subordinate Verb") +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and proficiency
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Correct responses on DELE proficiency measure", y = "Proportion of subjunctive responses", caption = "", 
       title = "Subjunctive by Proficiency, EMPT", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = DELE, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(25,50)) +
  ylim(c(0,1)) +
  labs(x = "Correct responses on DELE proficiency measure", y = "Proportion of subjunctive responses", caption = "", 
       title = "Subjunctive by Proficiency, MPST", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))


## By average accuracy and use
Subjunctive_EPT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Proportion of subjunctive responses",
       title = "Subjunctive by Frequency of Use, EMPT", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))

Subjunctive_FCT_Aggregate %>%
  ggplot(aes(x = FofA, y = Part_Avg, color = ExpGroup)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(5,50)) +
  ylim(c(0,1)) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Proportion of subjunctive responses",
       title = "Subjunctive by Frequency of Use, MPST", color = "Group") +
  theme(plot.title = element_text(hjust = 0.5))


## By age of acquisition
Heritage_EPT_Standardized %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  ylim(c(0,1)) +
  labs(x = "Age of acquisition of English", y = "Proportion of subjunctive responses", caption = "", 
       title = "Subjunctive by Age of Acquisition, EMPT") +
  theme(plot.title = element_text(hjust = 0.5))

Heritage_FCT_Standardized %>%
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlim(c(0,8)) +
  ylim(c(0,1)) +
  labs(x = "Age of acquisition of English", y = "Proportion of subjunctive responses", caption = "", 
       title = "Subjunctive by Age of Acquisition, MPST") +
  theme(plot.title = element_text(hjust = 0.5))
