# Libraries
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(broom)
library(broomExtra)

options(scipen = 99)


# Load dataframes
EPT_Original <- read_csv("../CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
FCT_Original <- read_csv("../CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
Composite_Original <- read_csv("../CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")


# Left bind data
EPT_Modified <- EPT_Original %>% 
  left_join()