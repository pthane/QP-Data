# Prepare environment
library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)
library(broom)
library(broomExtra)

options(scipen = 99)

# Load dataframe
EPT_Aspect_Data = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
FCT_Aspect_Data = read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")


# Create models
## Production
Aspect_Production = glmer(
  Response ~ Token_Main_Std * Reg_Numerical * DELE_Std +
    (1 | Participant_ID) + (1 | Item),
  data = EPT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Production)


## Interpretation
Aspect_Selection = glmer(
  Response ~ Token_Main_Std * Reg_Numerical * DELE_Std +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Selection)


## Fit model with morphological factors only
Aspect_Selection_Morphology = glmer(
  Response ~ Token_Main_Std * Reg_Numerical +
    (1 | Participant_ID) + (1 | Item),
  data = FCT_Aspect_Data, family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(Aspect_Selection_Morphology)





# Nested model comparisons
## Production Model
NMC_Prod_Null <- glmer(Response ~ 1 +
                    (1 | Participant_ID) + (1 | Item),
                  family = binomial,
                  data = EPT_Aspect_Data)

NMC_Prod_Token <- glmer(Response ~ 1 + Token_Main_Std +
                     (1 | Participant_ID) + (1 | Item),
                   family = binomial,
                   data = EPT_Aspect_Data)

NMC_Prod_Regularity <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical +
                          (1 | Participant_ID) + (1 | Item),
                        family = binomial,
                        data = EPT_Aspect_Data)

NMC_Prod_DELE <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical + DELE_Std +
                    (1 | Participant_ID) + (1 | Item),
                  family = binomial,
                  data = EPT_Aspect_Data)

NMC_Prod_Morphology_Interaction <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical + Token_Main_Std:Reg_Numerical +
                                      (1 | Participant_ID) + (1 | Item),
                                    family = binomial,
                                    data = EPT_Aspect_Data)

NMC_Prod_Triple_Interaction <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical + DELE_Std + Token_Main_Std:Reg_Numerical + Token_Main_Std:Reg_Numerical:DELE_Std +
                                  (1 | Participant_ID) + (1 | Item),
                                family = binomial,
                                data = EPT_Aspect_Data)


anova(NMC_Prod_Null, NMC_Prod_Token, NMC_Prod_Regularity, NMC_Prod_Morphology_Interaction)
anova(NMC_Prod_Null, NMC_Prod_Token, NMC_Prod_Regularity, NMC_Prod_DELE, NMC_Prod_Morphology_Interaction, NMC_Prod_Triple_Interaction, test = "Chisq")


## Selection
NMC_Sel_Null <- glmer(Response ~ 1 +
                    (1 | Participant_ID) + (1 | Item),
                  family = binomial,
                  data = FCT_Aspect_Data)

NMC_Sel_Token <- glmer(Response ~ 1 + Token_Main_Std +
                     (1 | Participant_ID) + (1 | Item),
                   family = binomial,
                   data = FCT_Aspect_Data)

NMC_Sel_Regularity <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical +
                          (1 | Participant_ID) + (1 | Item),
                        family = binomial,
                        data = FCT_Aspect_Data)

NMC_Sel_DELE <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical + DELE_Std +
                    (1 | Participant_ID) + (1 | Item),
                  family = binomial,
                  data = FCT_Aspect_Data)

NMC_Sel_Morphology_Interaction <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical + Token_Main_Std:Reg_Numerical +
                                      (1 | Participant_ID) + (1 | Item),
                                    family = binomial,
                                    data = FCT_Aspect_Data)

NMC_Sel_Triple_Interaction <- glmer(Response ~ 1 + Token_Main_Std + Reg_Numerical + DELE_Std + Token_Main_Std:Reg_Numerical + Token_Main_Std:Reg_Numerical:DELE_Std +
                                  (1 | Participant_ID) + (1 | Item),
                                family = binomial,
                                data = FCT_Aspect_Data)


## Comprehension NMCs
anova(NMC_Sel_Null, NMC_Sel_Token, NMC_Sel_Regularity, NMC_Sel_Morphology_Interaction)
anova(NMC_Sel_Null, NMC_Sel_Token, NMC_Sel_Regularity, NMC_Sel_DELE, NMC_Sel_Morphology_Interaction, NMC_Sel_Triple_Interaction, test = "Chisq")


# Plot models
## Full model
plot_model(Aspect_Production, show.values = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_x_discrete(labels = c("Token : Regularity : Use", "Regularity : Use", "Token : Use", "Token : Regularity", "Frequency of Use", "Regularity", "Token Frequency")) + 
  labs(title = "Full Production Model", y = "β Estimates") +
  theme(plot.title = element_text(hjust = 0.5))

## Selection
plot_model(Aspect_Selection, show.values = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_x_discrete(labels = c("Token : Regularity : Use", "Regularity : Use", "Token : Use", "Token : Regularity", "Frequency of Use", "Regularity", "Token Frequency")) + 
  labs(title = "Full Selection Model", y = "β Estimates") +
  theme(plot.title = element_text(hjust = 0.5))
