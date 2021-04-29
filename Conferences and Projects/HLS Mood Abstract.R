library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(broom)
library(broomExtra)

options(scipen = 99)


# Load databases
EPT_Original <- read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
Matrix_Ratings <- read_csv("./CSV Files/Lexical Item Analysis/Matrix Verb List.csv")
Subordinate_Ratings <- read_csv("./CSV Files/Lexical Item Analysis/Subordinate Verb List.csv")


# Prepare production data
EPT_Mod <- merge(EPT_Original, Matrix_Ratings, by = "MainVerb", all.x = TRUE)

EPT_Final <- merge(EPT_Mod, Subordinate_Ratings, by = "SubVerb", all.x = TRUE) %>%
  mutate(Sub_SRLF_Std = (Sub_SRLF - mean(Sub_SRLF))/sd(Sub_SRLF),
         Sub_SRLF_Double = (Sub_SRLF + Sub_SRLF),
         Matrix_SRLF_Std = (Matrix_SRLF - mean(Matrix_SRLF))/sd(Matrix_SRLF),
         Matrix_SRLF_Double = (Matrix_SRLF + Matrix_SRLF))


# Production model
Mood_Production <- glmer(
  Response ~ DELE_Std + Sub_SRLF_Std + Matrix_SRLF_Std + DELE_Std:Sub_SRLF_Std + DELE_Std:Matrix_SRLF_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Final, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Mood_Production)

# Prepare subjunctive data
## Bind lexical ratings
EPT_Mood_Mod <- merge(EPT_Mood_Original, Matrix_Ratings, by = "MainVerb", all.x = TRUE)

EPT_Mood_Final <- merge(EPT_Mood_Mod, Subordinate_Ratings, by = "SubVerb", all.x = TRUE) %>%
  mutate(Sub_SRLF_Std = (Sub_SRLF - mean(Sub_SRLF))/sd(Sub_SRLF),
         Sub_SRLF_Double = (Sub_SRLF + Sub_SRLF),
         Matrix_SRLF_Std = (Matrix_SRLF - mean(Matrix_SRLF))/sd(Matrix_SRLF),
         Matrix_SRLF_Double = (Matrix_SRLF + Matrix_SRLF))

## Generate proficiency groups with 2 levels
EPT_Mood_Final_Advanced <- EPT_Mood_Final %>% 
  filter(DELE > "40") %>% 
  mutate(BinaryProf = "Advanced")

EPT_Mood_Final_Intermediate <- EPT_Mood_Final %>% 
  filter(DELE < "40") %>% 
  mutate(BinaryProf = "Intermediate")

## Generate participant averages for intermediate group
EPT_Mood_Final_PartAvg_Intermediate <- aggregate(EPT_Mood_Final_Intermediate$Response, list(EPT_Mood_Final_Intermediate$Item), FUN = mean, na.rm = TRUE)
EPT_Mood_Final_PartAvg_Intermediate <- EPT_Mood_Final_PartAvg_Intermediate %>% rename(Part_Avg = x)
EPT_Mood_Final_PartAvg_Intermediate <- left_join(EPT_Mood_Final_Intermediate, EPT_Mood_Final_PartAvg_Intermediate, by = c("Item" = "Group.1"))

## Generate participant averages for advanced group
EPT_Mood_Final_PartAvg_Advanced <- aggregate(EPT_Mood_Final_Advanced$Response, list(EPT_Mood_Final_Advanced$Item), FUN = mean, na.rm = TRUE)
EPT_Mood_Final_PartAvg_Advanced <- EPT_Mood_Final_PartAvg_Advanced %>% rename(Part_Avg = x)
EPT_Mood_Final_PartAvg_Advanced <- left_join(EPT_Mood_Final_Advanced, EPT_Mood_Final_PartAvg_Advanced, by = c("Item" = "Group.1"))

## Re-bind lists and reorder for facet grids
EPT_Mood_Final_PartAvg <- rbind(EPT_Mood_Final_PartAvg_Intermediate, EPT_Mood_Final_PartAvg_Advanced)

EPT_Mood_Final_PartAvg$ProfGroup <- factor(EPT_Mood_Final_PartAvg$ProfGroup,
                                           levels = c("Intermediate", "Advanced"))


# Subjunctive Figures
## Formatted graph for subordinate verb data
EPT_Mood_Final_PartAvg %>%
  ggplot(aes(x = Sub_SRLF_Std, y = Part_Avg, color = ProfGroup)) +
  geom_hline(yintercept = 0.5, color = "white", size = 2) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     limits = c(0, 1)) +
  guides(color = FALSE, size = FALSE) +
  labs(x = "Self-rated lexical frequency", y = "Proportion of accurate responses", title = "Subjunctive Subordinate Verbs") +
  theme_grey(base_size = 16, base_family = 'Times') +
  theme(plot.title = element_text(hjust = 0.5))


## Formatted graph for matrix verb data
EPT_Mood_Final_PartAvg %>%
  ggplot(aes(x = Matrix_SRLF_Std, y = Part_Avg, color = ProfGroup)) +
  geom_hline(yintercept = 0.5, color = "white", size = 2) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     limits = c(0, 1)) +
  guides(color = FALSE, size = FALSE) +
  labs(x = "Self-rated lexical frequency", y = "Proportion of accurate responses", title = "Subjunctive Matrix Verbs") +
  theme_grey(base_size = 16, base_family = 'Times') +
  theme(plot.title = element_text(hjust = 0.5))