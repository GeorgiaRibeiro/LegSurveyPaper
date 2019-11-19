#--------------------------------------------------#
#     Artigo Political Methods Uruguai Nov/2019    #
#       Georgia Ribeiro Git: @georgiaribeiro       #
#                ~ Análise BLS                     # 
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
autopos_bls = data.frame(bls_2013 %>%
                           select(lrclass) %>%
                           group_by(partido = bls_2013$party_survey))

#Excluindo não respostas
autopos_bls = filter(autopos_bls, lrclass != "-999")

#recodificar nomes dos partidos e nome coluna
nomes_partidos = read.csv("coding_partidos_bls.csv", sep = ";")

autopos_bls$partido = factor(autopos_bls$partido, 
                              levels = nomes_partidos$codigo, 
                              labels = nomes_partidos$nome)
autopos_bls$partido = as.character(autopos_bls$partido)

colnames(autopos_bls) = c('escala', 'partido')

#alterar tipo da variavel e subtrair 1 pra voltar ao valor original
autopos_bls$escala = as.integer(autopos_bls$escala)
autopos_bls$escala = autopos_bls$escala -1

#adicionar identificação do survey
autopos_bls$survey = 'BLS Autoposicionamento'

#media por partido
autopos_bls %>% group_by(partido) %>%
                summarise(autopos_bls = mean(escala))

  # ~ Calculo para moda
  statmod = function(x) {
    z = table(as.vector(x)); names(z)[z == max(z)]
  }
  
#moda por partido

    
#--------- Posicionamento dos demais partidos (BLS) ---------#
#carregar banco
partidos = data.frame(select(bls_2013, lrpmdb:lrpv))
glimpse(partidos)

#alterar tipo de todas as colunas
partidos[]=lapply(partidos, as.numeric)

#excluir não respostas
partidos = partidos %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  filter_all(all_vars(. != "-999"))

# ~~~ PARTE 1 ~~~#
#alterar tipo do banco para fazer boxplot
pos_bls = gather(partidos, 'partido', 'escala', 1:14)

#padronizar nomes partidos
pos_bls$partido = str_remove(pos_bls$partido, "lr")
pos_bls$partido = str_to_upper(pos_bls$partido)

#adicionar identificação do survey
pos_bls$survey = 'BLS'

# ~~~ PARTE 2 ~~~#
#criar banco com estat. descritiva para comparar [1/2]
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

#agrupar estatisticas em df [2/2]
bls2013 = mutate(bls2013, media_posbls = df)
bls2013 = mutate(bls2013, min_posbls = dfmin)
bls2013 = mutate(bls2013, max_posbls = dfmax)
bls2013 = mutate(bls2013, desvio_posbls = dfdp)
bls2013 = mutate(bls2013, moda_posbls = dfmd)

#--------- Posicionamento partidário (ABCP) ---------#
#carregar banco original
abcp2010 = read.csv("abcp2010.csv", sep = ";")
abcp2010 = gather(abcp2010, 'partido', 'escala', 2:28)

  #padronizar estilo do banco
abcp2010 = abcp2010[19:20]
abcp2010$survey = 'ABCP 2010'

  #transformar escala 7-pontos para 10-pontos
abcp2010$escala10 = as.numeric(format((abcp2010$escala * 10/7),
                  digits = 0, format = "f"))
  #renomear colunas para combinar bancos
colnames(abcp2010) = c('partido', 'escala7', 'survey', 'escala')

#carregar banco de estatísticas
abcp_stat = read.csv("ABCP2010_survey.csv", sep = ";", stringsAsFactors = FALSE)
abcp_stat$media = as.numeric(gsub("\\,", ".", abcp_stat$media), digits=3)
str(abcp_stat)

#= = = = = = Comparações = = = = = = #

# 1. Autoposicionamento X Posição demais partidos (BLS)
#identificar partidos comuns
part_comuns1 = inner_join(pos_bls, autopos_bls, by = 'partido')
table(part_comuns1$partido)

#unir bancos
comp1 = rbind(pos_bls, autopos_bls)

#filtrar partidos comuns
comp1 = comp1 %>%
  filter(partido %in% c("PDT", "PMDB","PSB", "PSD","PSDB", "PSOL", "PT", "PTB"))
View(comp1)

t.test(comp1$media_posbls, comp1$media_autopos)

#boxplot
box_comp1 = ggplot(comp1, aes(x = partido, y = escala, fill = survey)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Posicionamento na escala ideológica",
                     breaks = seq(0, 10, 1),
                     limits=c(0, 10)) +
  scale_x_discrete(name = "Partido") +
  labs( title = "Classificação ideológica dos partidos ",
        subtitle = "Comparação posicionamento de partidos e autoposicionamento") +
  themeu()

box_comp1 + ggsave("comp_bls.png",
                path = "graficos",
                width = 7, height = 4, units = "in")


# 2. Posição partidos bls X abcp
#identificar partidos comuns
part_comuns2 = inner_join(pos_bls, abcp2010, by = 'partido')
table(part_comuns2$partido)

#alterar df abcp para corresponder [1/2]
abcp = data.frame(partido = abcp2010$partido,
                  escala = abcp2010$escala,
                  survey = abcp2010$survey)
#unir bancos [2/2]
comp2 = rbind(pos_bls, abcp)

#filtrar partidos comuns
comp2 = comp2 %>%
  filter(partido %in% c("DEM", "PDT","PMDB", "PPS","PR",
                        "PSB", "PSDB", "PSOL", "PT", "PTB", "PV"))
View(comp2)

#boxplot
box_comp2 = ggplot(comp2, aes(x = partido, y = escala, fill = survey)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Posicionamento na escala ideológica",
                     breaks = seq(0, 10, 1),
                     limits=c(0, 10)) +
  scale_x_discrete(name = "Partido") +
  labs(title = "Classificação ideológica dos partidos",
       subtitle = "Comparação entre tipos de surveys") +
  themeu()

box_comp2 + ggsave("comp_bls-abcp.png",
                   path = "graficos",
                   width = 7, height = 4, units = "in")
