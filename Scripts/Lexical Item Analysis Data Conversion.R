# Run packages
library(tidyverse)
library(effects)
library(ggeffects)
library(lme4)
library(lmerTest)
library(sjPlot)

options(scipen = 99)

#Preparation of data
## Load databases
Lexical_Item_Report = read.csv("./CSV Files/Lexical Item Analysis/Self-Reported Use Raw Data.csv")


## Generate standardized data
Lexical_Item_Report_Standardized = Lexical_Item_Report %>%
  mutate(amar_mean = mean(amar),
        atar_mean = mean(atar),
        bailar_mean = mean(bailar),
        conseguir_mean = mean(conseguir),
        creer_mean = mean(creer),
        dejar_mean = mean(dejar),
        doler_mean = mean(doler),
        enviar_mean = mean(enviar),
        esperar_mean = mean(esperar),
        estar_mean = mean(estar),
        excluir_mean = mean(excluir),
        faltar_mean = mean(faltar),
        gustar_mean = mean(gustar),
        haber_mean = mean(haber),
        necesitar_mean = mean(necesitar),
        ordenar_mean = mean(ordenar),
        tener_mean = mean(tener),
        tomar_mean = mean(tomar),
        tratar_mean = mean(tratar),
        valorar_mean = mean(valorar),
        viajar_mean = mean(viajar),
        vivir_mean = mean(vivir))