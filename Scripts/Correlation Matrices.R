# Load packages
library(tidyverse)
library(lme4)
library(corrplot)

# Load data
HS_Aspect_EPT <- read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  select(ExpLI, Token_Main_Std, LOR_Std, AoA_ENG_Std, FofA_Std, DELE_Std) %>%
  na.omit

HS_Aspect_FCT <- read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv") %>% 
  select(ExpLI, Token_Main_Std, LOR_Std, AoA_ENG_Std, FofA_Std, DELE_Std) %>% 
  na.omit

HS_Mood_EPT <- read_csv("./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv") %>% 
  select(ExpLI, Token_Sub_Std, Token_Main_Std, LOR_Std, AoA_ENG_Std, FofA_Std, DELE_Std) %>%
  na.omit

HS_Mood_FCT <- read_csv("./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv") %>% 
  select(ExpLI, Token_Sub_Std, Token_Main_Std, LOR_Std, AoA_ENG_Std, FofA_Std, DELE_Std) %>% 
  na.omit

L2_Aspect_EPT <- read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv") %>% 
  select(ExpLI, Token_Main_Std, LOR_Std, FofA_Std, DELE_Std) %>%
  na.omit

L2_Aspect_FCT <- read_csv("./CSV Files/L2 Learners/L2 Learners FCT Preterit Data.csv") %>% 
  select(ExpLI, Token_Main_Std, LOR_Std, FofA_Std, DELE_Std) %>% 
  na.omit

L2_Mood_EPT <- read_csv("./CSV Files/L2 Learners/L2 Learners EPT Subjunctive Data.csv") %>% 
  select(ExpLI, Token_Sub_Std, Token_Main_Std, LOR_Std, FofA_Std, DELE_Std) %>%
  na.omit

L2_Mood_FCT <- read_csv("./CSV Files/L2 Learners/L2 Learners FCT Subjunctive Data.csv") %>% 
  select(ExpLI, Token_Sub_Std, Token_Main_Std, LOR_Std, FofA_Std, DELE_Std) %>% 
  na.omit


# Explore HS production correlations
HS_Aspect_EPT_Correlation <- cor(HS_Aspect_EPT)
corrplot(HS_Aspect_EPT_Correlation, type = "upper")

HS_Mood_EPT_Correlation <- cor(HS_Mood_EPT)
corrplot(HS_Mood_EPT_Correlation, type = "upper")

# Explore HS comprehension correlations
HS_Aspect_FCT_Correlation <- cor(HS_Aspect_FCT)
corrplot(HS_Aspect_FCT_Correlation, type = "upper")

HS_Mood_FCT_Correlation <- cor(HS_Mood_FCT)
corrplot(HS_Mood_FCT_Correlation, type = "upper")


# Explore L2 production correlations
L2_Aspect_EPT_Correlation <- cor(L2_Aspect_EPT)
corrplot(L2_Aspect_EPT_Correlation, type = "upper")

L2_Mood_EPT_Correlation <- cor(L2_Mood_EPT)
corrplot(L2_Mood_EPT_Correlation, type = "upper")

# Explore L2 comprehension correlations
L2_Aspect_FCT_Correlation <- cor(L2_Aspect_FCT)
corrplot(L2_Aspect_FCT_Correlation, type = "upper")

L2_Mood_FCT_Correlation <- cor(L2_Mood_FCT)
corrplot(L2_Mood_FCT_Correlation, type = "upper")
