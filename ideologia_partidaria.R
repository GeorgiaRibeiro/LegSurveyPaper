#-------------------------------------------#
# Artigo Political Methods Uruguai Nov/2019 #
#   Georgia Ribeiro Git: @georgiaribeiro    #
#-------------------------------------------#

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

#--------- ajustando banco ---------#
#carregar banco
load(file = "BLS8.RData")
glimpse(bls_2013)

#Filtrar por onda do survey
bls_2013 = filter(bls, wave == 2013)

#definindo tipo das variaveis
bls_2013$party_survey=as.character(bls_2013$party_survey)
bls_2013$region=as.character(bls_2013$region)
bls_2013$female=as.factor(bls_2013$female)

bls_2013$lrclass=as.factor(bls_2013$lrclass)


#--------- Autoposicionamento ---------#
lrclass_resp = data.frame(bls_2013 %>%
                            select(lrclass)  %>%
                            group_by(partido = bls_2013$party_survey))

#Excluindo não respostas
lrclass_resp = filter(lrclass_resp, lrclass != "-999")

#atualizando label das respostas - falhou (Como alterar labels de vários códigos?)
i = 1
#lrclass_resp %>%
  while(i<=ncol(lrclass_resp$lrclass)) {
    lrclass_resp$lrclass[[i]] = factor(lrclass_resp$lrclass[[i]],
                             labels = c("Extrema esquerda", "2", "3", "4", "5", "6", "7", "8", "9", "Extrema direita"),
                             levels=c(1:10))
  i = i + 1
}


#--------- Autoposicionamento/Analise ---------#
prop.table(table(lrclass_resp$lrclass))

glimpse(lrclass_resp)

#gambiarra
lrclass_resp = mutate(lrclass_resp, lrclass2 = as.integer(lrclass_resp$lrclass))
lrclass_resp$lrclass2 = lrclass_resp$lrclass2 - 1


lrc_graf = data.frame(lrclass_resp %>%
                     group_by(partido) %>%
                     mutate(prop = prop.table(lrclass2)))

            
#--------- Autoposicionamento/graficos ---------#
lrclass_resp_plot = likert(lrclass_resp)
g2 = plot(lrclass_resp_plot, centered = FALSE, include.histogram = FALSE) + 
  ggtitle("Autoposicionamento ideológico") + 
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"))+
  theme(legend.key.size=unit(0.02,"npc"))+guides(fill=guide_legend(""))

#grafico por filiação partidária
g3 = ggplot(lrc_graf, aes(x=partido,y=prop,fill=lrclass, label=prop, (x = reorder(prop, desc(prop)))))+
  geom_bar(stat = "identity", position = "fill")

g3+theme_minimal()+
  coord_flip()+
  labs(x = "Partido",
       y = "Posicionamento Ideologico",
       fill = "Escala",
       title = "Autoposicionamento ideoloógico por Filiação Partidária",
       subtitle = "Brazilian Legislative Survey, ano 2013",
       caption = "Fonte: Elaboração própria") 

#grafico por partido

#--------- Posicionamento dos Partidos ---------#
partidos = data.frame(select(bls_2013, lrpmdb:lrpv))
glimpse(partidos)

partidos[]=lapply(partidos, as.numeric)

partidos = partidos %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  filter_all(all_vars(. != "-999"))

#carregar banco do survey abcp
abcp2010 = read.csv("ABCP2010_survey.csv", sep = ";", stringsAsFactors = FALSE)
abcp2010$media = as.numeric(gsub("\\,", ".", abcp2010$media), digits=3)
str(abcp2010)

#criar df para comparar [1/2]
party = names(partidos)
bls2013 = data.frame(partido = str_to_upper(str_remove(party,"lr")))
bls2013$partido = as.character(bls2013$partido)


#media posicionamento ideologico por partido
df = vector("double", ncol(partidos))
names(df) = names(partidos)
for (i in names(partidos)) {
  df[i] = mean(partidos[[i]])
}
df

#menor escala atribuida
dfmin = vector("double", ncol(partidos))
names(dfmin) = names(partidos)
for (i in names(partidos)) {
  dfmin[i] = min(partidos[[i]])
}
dfmin

#maior escala atribuida
dfmax = vector("double", ncol(partidos))
names(dfmax) = names(partidos)
for (i in names(partidos)) {
  dfmax[i] = max(partidos[[i]])
}
dfmax

#desvio padrao das respostas
dfdp = vector("double", ncol(partidos))
names(dfdp) = names(partidos)
for (i in names(partidos)) {
  dfdp[i] = sd(partidos[[i]])
}
dfdp

# ~ Calculo para moda
statmod = function(x) {
  z = table(as.vector(x)); names(z)[z == max(z)]
}

#moda das respostas
dfmd = vector("double", ncol(partidos))
names(dfmd) = names(partidos)
for (i in names(partidos)) {
  dfmd[i] = statmod(partidos[[i]])
}
dfmd

#criar df para comparar [2/2]
bls2013 = mutate(bls2013, media = df)
bls2013 = mutate(bls2013, min = dfmin)
bls2013 = mutate(bls2013, max = dfmax)
bls2013 = mutate(bls2013, desvio = dfdp)
bls2013 = mutate(bls2013, moda = dfmd)

#--------- Comparação BLS x ABCP ---------#
str(bls2013)
str(abcp2010)

comp_surveys = inner_join(bls2013, abcp2010, by="partido")
View(Teste)
