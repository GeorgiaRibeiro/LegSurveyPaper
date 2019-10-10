#-------------------------------------------#
# Artigo Political Methods Uruguai Nov/2019 #
#   Geórgia Ribeiro Git: @georgiaribeiro    #
#-------------------------------------------#

#---------------Resumo: cód variáveis--------------#
# wave: ano da aplicação do survey
# party_survey: partido filiado
# lrclass: autoposicionamento ideologico
# lrXXX: posicionamento ideologico do partido XXX
# -------------------------------------------------#


#carregar pacotes
library(tidyverse)
library(ggplot2)

#Importar e visualisar banco
glimpse(bls_2013)

bls_2013$party_survey <- as.character(bls_2013$party_survey)
bls_2013$region <- as.character(bls_2013$region)
bls_2013$female <- as.factor(bls_2013$female)

bls_2013$lrclass <- as.factor(bls_2013$lrclass)

bls_2013$visitas2 <- as.factor(bls_2013$visitas2)
bls_2013$pedidos2 <- as.factor(bls_2013$pedidos2)
bls_2013$destpp2 <- as.factor(bls_2013$destpp2)
bls_2013$pork2 <- as.factor(bls_2013$pork2)
bls_2013$destcn2 <- as.factor(bls_2013$destcn2)
bls_2013$entidad2 <- as.factor(bls_2013$entidad2)
bls_2013$tvradio <- as.factor(bls_2013$tvradio)
bls_2013$discplen <- as.factor(bls_2013$discplen)
bls_2013$votplen <- as.factor(bls_2013$votplen)

#Filtrar por onda do survey
bls_2013 <- filter(bls, wave == 2013)
dim(select_2013)

#autoposicionamento
lrclass2 <- subset(bls_2013, lrclass >= 0) ## excluir NA
glimpse(lrclass2)

sum(bls_2013$lrclass == "-999") ## checando p ver se excluir todos
mean(lrclass2, lrclass)


#classificação dos partidos
bls_2013 %>%
  select(lrpmdb:lrpv)

complete.cases(select)
#cruzar partido atual com autoposicioamento e comparar principais

#atividades parlamentares 
bls_2013 %>%
  select(visitas2, pedidos2, destpp2, pork2, destcn2, entidad2, local2, tvradio, discplen, votplen) 

ggplot(bls_2013, aes(visitas2)) +
  geom_bar()

bls_2013 %>%
  ggplot(aes(visitas2)) +
  geom_bar(aes(fill = vt2))

# frequencia por variável - Visitas: (OK)
vt2 = prop.table(table(bls_2013$visitas2))*100

barplot(vt2)

#frequencia de todas as variaveis (ERRO)
ativparl <- c(bls_2013$visitas2, bls_2013$pedidos2, bls_2013$destpp2, bls_2013$pork2,
              bls_2013$destcn2, bls_2013$entidad2, bls_2013$local2, bls_2013$tvradio)

for (a in ativparl){
  prop.table(a)
} 

#3a tentativa
proporcao=function(bls_2013){
  round(prop.table(table(bls_2013))*100,2)
}
apply(bls_2013,2,table)


#N SEI MAIS NEM O QUE TO TENTANDO
bls_2013 %>%
  for (i in ativparl) {
    summarise(prop.table(i))
  }

