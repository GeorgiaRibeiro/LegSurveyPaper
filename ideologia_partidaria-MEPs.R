#--------------------------------------------------#
#     Artigo Political Methods Uruguai Nov/2019    #
#       Georgia Ribeiro Git: @georgiaribeiro       #
#                ~ Análise MEPs                    # 
#--------------------------------------------------#

#---------------Resumo: cod variáveis--------------#
# q1_2: grupo do parlamento
# q1_3: partido nacional
# q4_1_1: autoposicionamento idelogico
# q4_1_2: posicionamento ideologico do seu partido
# q4_1_4: posicionamento ideologico do seu grupo
# -------------------------------------------------#

#carregar pacotes [1. Analises; 2. Visualização]
library(tidyverse)
library(plyr)
library(magrittr)
library(dplyr)

library(ggplot2)
library(plotly)
library(likert)

#carregar banco e codebook
meps2015 = read.csv("MEPs2015.csv", sep = ";")
meps_cds = read.csv("docs surveys/MEPs_codebook.csv", sep = ";", na.strings=c(""," ","NA"))

#faxina inicial (apagar linhas vazias)
meps_cds = meps_cds[rowSums(is.na(meps_cds)) != ncol(meps_cds),]

#renomear q1_2 (codigo -> nome)
meps2015$q1_2 = factor(autopos_bls$partido, 
                             levels = nomes_partidos$codigo, 
                             labels = nomes_partidos$nome)
autopos_bls$partido = as.character(autopos_bls$partido)
