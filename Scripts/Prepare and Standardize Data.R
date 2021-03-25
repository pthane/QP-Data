library(tidyverse)

# Load Data
EPT_Master <- read_csv("./CSV Files/EPT Master.csv")
FCT_Master <- read_csv("./CSV Files/FCT Master.csv")

# Separate by task
EPT_Aspect <- EPT_Master %>% 
  filter(Property == "Preterit")

EPT_Mood <- EPT_Master %>% 
  filter(Property == "Subjunctive")

FCT_Aspect <- FCT_Master %>% 
  filter(Property == "Preterit")

FCT_Mood <- FCT_Master %>% 
  filter(Property == "Subjunctive")


# Filter out "valorar" and "conseguir" from mood datasets
## Mood EPT (production task)
EPT_Mood_Esperar <- EPT_Mood %>% 
  filter(MainVerb == "#01: esperar")

EPT_Mood_Querer <- EPT_Mood %>% 
  filter(MainVerb == "#02: querer")

EPT_Mood_Permitir <- EPT_Mood %>% 
  filter(MainVerb == "#03: permitir")

EPT_Mood_Necesitar <- EPT_Mood %>% 
  filter(MainVerb == "#04: necesitar")

EPT_Mood_Pedir <- EPT_Mood %>% 
  filter(MainVerb == "#05: pedir")

EPT_Mood_Desear <- EPT_Mood %>% 
  filter(MainVerb == "#06: desear")

EPT_Mood_Sugerir <- EPT_Mood %>% 
  filter(MainVerb == "#07: sugerir")

EPT_Mood_Ordenar <- EPT_Mood %>% 
  filter(MainVerb == "#08: ordenar")

EPT_Mood_Adjusted <- rbind(EPT_Mood_Desear, EPT_Mood_Esperar, EPT_Mood_Necesitar, EPT_Mood_Ordenar, EPT_Mood_Pedir, EPT_Mood_Permitir, EPT_Mood_Querer, EPT_Mood_Sugerir)


## Mood FCT (listed as "MPST" in manuscript)
FCT_Mood_Esperar <- FCT_Mood %>% 
  filter(MainVerb == "#01: esperar")

FCT_Mood_Querer <- FCT_Mood %>% 
  filter(MainVerb == "#02: querer")

FCT_Mood_Permitir <- FCT_Mood %>% 
  filter(MainVerb == "#03: permitir")

FCT_Mood_Necesitar <- FCT_Mood %>% 
  filter(MainVerb == "#04: necesitar")

FCT_Mood_Pedir <- FCT_Mood %>% 
  filter(MainVerb == "#05: pedir")

FCT_Mood_Desear <- FCT_Mood %>% 
  filter(MainVerb == "#06: desear")

FCT_Mood_Sugerir <- FCT_Mood %>% 
  filter(MainVerb == "#07: sugerir")

FCT_Mood_Ordenar <- FCT_Mood %>% 
  filter(MainVerb == "#08: ordenar")

FCT_Mood_Adjusted <- rbind(FCT_Mood_Desear, FCT_Mood_Esperar, FCT_Mood_Necesitar, FCT_Mood_Ordenar, FCT_Mood_Pedir, FCT_Mood_Permitir, FCT_Mood_Querer, FCT_Mood_Sugerir)


# Create aggregate CSVs
Aggregate_Aspect <- rbind(EPT_Aspect, FCT_Aspect)
Aggregate_Mood <- rbind(EPT_Mood_Adjusted, FCT_Mood_Adjusted)


# Generate CSVs for comparison group
#### Note: this script removes any participants who arrived in the U.S. before adolescence (age 13).
EPT_Aspect_Comparison <- EPT_Aspect %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

EPT_Mood_Comparison <- EPT_Mood_Adjusted %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

FCT_Aspect_Comparison <- FCT_Aspect %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

FCT_Mood_Comparison <- FCT_Mood_Adjusted %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

Aggregate_Aspect_Comparison <- Aggregate_Aspect %>% 
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

Aggregate_Mood_Comparison <- Aggregate_Mood %>% 
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)


## Generate CSVs for HS
EPT_Aspect_Heritage <- EPT_Aspect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8) %>%

EPT_Mood_Heritage <- EPT_Mood_Adjusted %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8) %>%

FCT_Aspect_Heritage <- FCT_Aspect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

FCT_Mood_Heritage <- FCT_Mood_Adjusted %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

Aggregate_Aspect_Heritage <- Aggregate_Aspect %>% 
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

Aggregate_Mood_Heritage <- Aggregate_Mood %>% 
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)


## Generate CSVs for L2 Learners
EPT_Aspect_L2_Learners <- EPT_Aspect %>%
  filter(ExpGroup == "L2 Learner")

EPT_Mood_L2_Learners <- EPT_Mood_Adjusted %>%
  filter(ExpGroup == "L2 Learner")

FCT_Aspect_L2_Learners <- FCT_Aspect %>%
  filter(ExpGroup == "L2 Learner")

FCT_Mood_L2_Learners <- FCT_Mood_Adjusted %>%
  filter(ExpGroup == "L2 Learner")

Aggregate_Aspect_L2_Learners <- Aggregate_Aspect %>% 
  filter(ExpGroup == "L2 Learner")

Aggregate_Mood_L2_Learners <- Aggregate_Mood %>% 
  filter(ExpGroup == "L2 Learner")


# Standardize datasets for comparison group
## Aspect data
### Aspect EPT
EPT_Aspect_Comparison <- EPT_Aspect_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aspect FCT
FCT_Aspect_Comparison <- FCT_Aspect_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aggregate aspect
Aggregate_Aspect_Comparison <- Aggregate_Aspect_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_Comparison = EPT_Mood_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Mood FCT
FCT_Mood_Comparison = FCT_Mood_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate mood
Aggregate_Mood_Comparison <- Aggregate_Mood_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Standardize datasets for Heritage group
## Aspect data
### Aspect EPT
EPT_Aspect_Heritage <- EPT_Aspect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aspect FCT
FCT_Aspect_Heritage <- FCT_Aspect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aspect aggregate
Aggregate_Aspect_Heritage = Aggregate_Aspect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_Heritage = EPT_Mood_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Mood FCT
FCT_Mood_Heritage = FCT_Mood_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aggregate Mood
Aggregate_Mood_Heritage = Aggregate_Mood_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Standardize datasets for L2_Learners group
## Aspect data
### Aspect EPT
EPT_Aspect_L2_Learners <- EPT_Aspect_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aspect FCT
FCT_Aspect_L2_Learners <- FCT_Aspect_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aggregate FCT
Aggregate_Aspect_L2_Learners = Aggregate_Aspect_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_L2_Learners = EPT_Mood_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Mood FCT
FCT_Mood_L2_Learners = FCT_Mood_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Mood aggregate
Aggregate_Mood_L2_Learners = Aggregate_Mood_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Write revised CSV files
## Comparison
write_csv(EPT_Aspect_Comparison, "./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
write_csv(EPT_Mood_Comparison, "./CSV Files/Comparison/Comparison EPT Subjunctive Data.csv")
write_csv(FCT_Aspect_Comparison, "./CSV Files/Comparison/Comparison FCT Preterit Data.csv")
write_csv(FCT_Mood_Comparison, "./CSV Files/Comparison/Comparison FCT Subjunctive Data.csv")
write_csv(Aggregate_Aspect_Comparison, "./CSV Files/Comparison/Comparison Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_Comparison, "./CSV Files/Comparison/Comparison Aggregate Subjunctive Data.csv")

## Heritage
write_csv(EPT_Aspect_Heritage, "./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
write_csv(EPT_Mood_Heritage, "./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
write_csv(FCT_Aspect_Heritage, "./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
write_csv(FCT_Mood_Heritage, "./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
write_csv(Aggregate_Aspect_Heritage, "./CSV Files/Heritage/Heritage Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_Heritage, "./CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")

## L2 Learners
write_csv(EPT_Aspect_L2_Learners, "./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv")
write_csv(EPT_Mood_L2_Learners, "./CSV Files/L2 Learners/L2 Learners EPT Subjunctive Data.csv")
write_csv(FCT_Aspect_L2_Learners, "./CSV Files/L2 Learners/L2 Learners FCT Preterit Data.csv")
write_csv(FCT_Mood_L2_Learners, "./CSV Files/L2 Learners/L2 Learners FCT Subjunctive Data.csv")
write_csv(Aggregate_Aspect_L2_Learners, "./CSV Files/L2 Learners/L2 Learners Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_L2_Learners, "./CSV Files/L2 Learners/L2 Learners Aggregate Subjunctive Data.csv")