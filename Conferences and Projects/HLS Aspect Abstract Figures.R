library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)
library(broom)
library(broomExtra)

# Load databases
Preterit_EPT = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
Preterit_FCT = read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")


# Prepare aspect data
## Generate group/task averages by verb
Preterit_EPT_Modified = aggregate(Preterit_EPT$Response, list(Preterit_EPT$MainVerb), FUN = mean, na.rm = TRUE)
Preterit_EPT_Modified = Preterit_EPT_Modified %>% rename(Verb_Avg = x)
Preterit_EPT_Modified = left_join(Preterit_EPT, Preterit_EPT_Modified, by = c("MainVerb" = "Group.1"))

Preterit_FCT_Modified = aggregate(Preterit_FCT$Response, list(Preterit_FCT$MainVerb), FUN = mean, na.rm = TRUE)
Preterit_FCT_Modified = Preterit_FCT_Modified %>% rename(Verb_Avg = x)
Preterit_FCT_Modified = left_join(Preterit_FCT, Preterit_FCT_Modified, by = c("MainVerb" = "Group.1"))


## Re-bind/Reorder tasks for graphs
Preterit_Aggregate = rbind(Preterit_EPT_Modified, Preterit_FCT_Modified)

Preterit_Aggregate$Modality <- factor(Preterit_Aggregate$Modality,
                                             levels = c("Production", "Interpretation"))


# Create single proficiency average score
## Generate lists
EPT_Proficiency = aggregate(Preterit_EPT$Response, list(Preterit_EPT$Participant_ID), FUN = mean, na.rm = TRUE)
EPT_Proficiency = EPT_Proficiency %>% rename(Part_Avg = x)
EPT_Proficiency = left_join(Preterit_EPT, EPT_Proficiency, by = c("Participant_ID" = "Group.1"))

FCT_Proficiency = aggregate(Preterit_FCT$Response, list(Preterit_FCT$Participant_ID), FUN = mean, na.rm = TRUE)
FCT_Proficiency = FCT_Proficiency %>% rename(Part_Avg = x)
FCT_Proficiency = left_join(Preterit_FCT, FCT_Proficiency, by = c("Participant_ID" = "Group.1"))


## Re-bind and reorder tasks for facet grids
Preterit_Proficiency <- rbind(EPT_Proficiency, FCT_Proficiency)

Preterit_Proficiency$Modality <- factor(Preterit_Proficiency$Modality,
                                           levels = c("Production", "Interpretation"))


# Aspect Figures
## Plot all items
Preterit_Aggregate %>%
  ggplot(aes(x = log(Token_Main_Lemma), y = Verb_Avg, color = Modality)) + 
  geom_point() +
  geom_smooth(method = glm) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Log-transformed lexical frequency in Davies (2016)", y = "Proportion of preterit responses", color = "Task", 
       title = "Preterit by Lexical Frequency and Task") +
  theme(plot.title = element_text(hjust = 0.5))

## Plot proficiency effect
Preterit_Proficiency %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = glm) +
  facet_grid(cols = vars(Modality)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "DELE Proficiency Score", y = "Proportion of preterit responses by partici", color = "Token Frequency", 
       title = "Proportion of Preterit by Task and Profociency") +
  theme(plot.title = element_text(hjust = 0.5))
