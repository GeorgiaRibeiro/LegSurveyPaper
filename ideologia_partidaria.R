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
library(modeest)

library(ggplot2)
library(plotly)
library(likert)

#pacote para calcular media

#--------- ajustando banco ---------#
#carregar banco
bls = load("BLS8.RData")
glimpse(bls_2013)

#Filtrar por onda do survey
bls_2013 = filter(bls, wave == 2013)

#definindo tipo das variaveis
bls_2013$party_survey=as.character(bls_2013$party_survey)
bls_2013$region=as.character(bls_2013$region)
bls_2013$female=as.factor(bls_2013$female)

bls_2013$lrclass=as.factor(bls_2013$lrclass)


#--------- Autoposicionamento (BLS) ---------#
lrclass_resp = data.frame(bls_2013 %>%
                            select(lrclass)  %>%
                            group_by(partido = bls_2013$party_survey))

#Excluindo não respostas
lrclass_resp = filter(lrclass_resp, lrclass != "-999")

#recodificar nomes dos partidos
nomes_partidos = read.csv("coding_partidos_bls.csv", sep = ";")

lrclass_resp$partido = factor(lrclass_resp$partido, 
                              levels = nomes_partidos$codigo, 
                              labels = nomes_partidos$nome)
lrclass_resp$partido = as.character(lrclass_resp$partido)

#alterar tipo da variavel e subtrair 1 pra voltar ao valor original
lrclass_resp$lrclass = as.integer(lrclass_resp$lrclass)
lrclass_resp$lrclass = lrclass_resp$lrclass -1

#media por partido
autopos_bls2013 = lrclass_resp %>% group_by(partido) %>%
                summarise(media_autopos = mean(lrclass))

  # ~ Calculo para moda
  statmod = function(x) {
    z = table(as.vector(x)); names(z)[z == max(z)]
  }
  
#moda por partido
?
    
#--------- Posicionamento dos demais partidos (BLS) ---------#
#carregar banco
partidos = data.frame(select(bls_2013, lrpmdb:lrpv))
glimpse(partidos)

partidos[]=lapply(partidos, as.numeric)

partidos = partidos %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  filter_all(all_vars(. != "-999"))

#criar df para comparar [1/2]
bls2013 = data.frame(partido = str_to_upper(str_remove(names(partidos),"lr")))
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

#moda das respostas
dfmd = vector("double", ncol(partidos))
names(dfmd) = names(partidos)
for (i in names(partidos)) {
  dfmd[i] = statmod(partidos[[i]])
}
dfmd

#criar df para comparar [2/2]
bls2013 = mutate(bls2013, media_posbls = df)
bls2013 = mutate(bls2013, min_posbls = dfmin)
bls2013 = mutate(bls2013, max_posbls = dfmax)
bls2013 = mutate(bls2013, desvio_posbls = dfdp)
bls2013 = mutate(bls2013, moda_posbls = dfmd)

#--------- Posicionamento partidário (ABCP) ---------#
#carregar banco do survey abcp
abcp2010 = read.csv("ABCP2010_survey.csv", sep = ";", stringsAsFactors = FALSE)
abcp2010$media = as.numeric(gsub("\\,", ".", abcp2010$media), digits=3)
str(abcp2010)

#= = = = = = Comparações = = = = = = #

# 1. Autoposicionamento X Posição demais partidos (BLS)
#identificar partidos comuns aos surveys
comp1 = inner_join(bls2013, autopos_bls2013, by="partido", all = T)
View(comp2)

t.test(comp1$media_posbls, comp1$media_autopos)

# 2. Posição partidos bls X abcp
#identificar partidos comuns aos surveys
comp2 = inner_join(bls2013, abcp2010, by="partido")
View(comp2)

#adaptar df para grafico agrupado (grouped bars)
survey_bls = comp2[1:6]
survey_bls = mutate(survey_bls, survey = "BLS 2013")
survey_bls = mutate(survey_bls,
                    medias = media_posbls, modas = moda_posbls,
                    mins = min_posbls, maxs = max_posbls, desvios= desvio_posbls)
survey_bls[2:6] = NULL

survey_abcp = data.frame(comp2[1], comp2[7:10], comp2[12])
survey_abcp = mutate(survey_abcp, survey = "ABCP 2010")
survey_abcp = mutate(survey_abcp,
                     medias = media.y, modas = moda.y,
                     mins = min.y, maxs = max.y, desvios= desvio.y)
survey_abcp[2:6] = NULL

survey_all = rbind(survey_bls, survey_abcp)
str(survey_all)
survey_all$modas = as.numeric(survey_all$modas)


#--------- Gráfico Comparação BLS x ABCP (Versão antiga)---------#
#ordem crescente ABCP 2010 [media e moda]
survey_all$partido_crescente = reorder(
  survey_all$partido,
  X = -(survey_all$medias * (survey_all$survey == "ABCP 2010"))
)

survey_all$partido_crescente2 = reorder(
  survey_all$partido,
  X = -(survey_all$medias * (survey_all$survey == "ABCP 2010"))
)

#grafico medias
g_media = ggplot(survey_all, aes(x = partido_crescente, y= medias, fill=survey)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() +
 #scale_x_continuous(limits = c(1, 10))+
  scale_fill_manual(values=c("darksalmon", "darkred"))+
  theme_minimal()+
  labs(x = "",
       y = "Posicionamento ideológico (médias)",
       fill = "Surveys",
       title = "Comparação do posicionamento ideológico entre surveys",
       caption = "Fonte: Elaboração própria") +
  geom_text(aes(label=format(medias, digits=2)),
              position = position_dodge(width = 0.9), 
            hjust = -0.1,
            size = 2.75,
            colour = 'gray12')

print(g_moda)


#grafico moda
g_moda = ggplot(survey_all, aes(x = partido_crescente2, y= modas, fill=survey, width=0.8)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() +
  #scale_x_continuous(limits = c(1, 10))+
  scale_fill_manual(values=c("darksalmon", "darkred"))+
  theme_minimal()+
  labs(x = "",
       y = "Posicionamento ideológico (moda)",
       fill = "Surveys",
       title = "Comparação do posicionamento ideológico entre surveys",
       caption = "Fonte: Elaboração própria") +
  geom_text(aes(label= modas),
            position = position_dodge(width = 0.9), 
            hjust = -0.1,
            size = 2.75,
            colour = 'gray12')

print(g_moda)
