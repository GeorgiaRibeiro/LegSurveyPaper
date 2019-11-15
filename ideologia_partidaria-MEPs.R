#--------------------------------------------------#
#     Artigo Political Methods Uruguai Nov/2019    #
#       Georgia Ribeiro Git: @georgiaribeiro       #
#                ~ Análise MEPs                    # 
#--------------------------------------------------#

#---------------Resumo: cod variáveis--------------#
# wave: ano da aplicação do survey
# party_survey: partido filiado
# lrclass: autoposicionamento ideologico
# lrXXX: posicionamento ideologico do partido XXX
# -------------------------------------------------#

#carregar pacotes [1. Analises; 2. Visualização]
library(tidyverse)
library(plyr)
library(magrittr)
library(dplyr)

library(ggplot2)
library(plotly)
library(likert)
