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

#carregar pacotes
library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)

#Banco e variáveis
glimpse(bls_2013)

#definindo tipo das variaveis
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

#1. IDEOLOGIA PARTIDARIA
#autoposicionamento
lrclass2 <- subset(bls_2013, lrclass >= 0) ## excluir NA
glimpse(lrclass2)

sum(bls_2013$lrclass == "-999") ## checando p ver se excluir todos
#~mean(lrclass2, lrclass)


#classificação dos partidos
bls_2013 %>%
  select(lrpmdb:lrpv)

#~complete.cases()
#cruzar partido atual com autoposicioamento e comparar principais


#2. ATIVIDADES PARLAMENTARES
#Atividades: visitas2, pedidos2, destpp2, pork2, destcn2, entidad2, local2, tvradio, discplen, votplen) 

#somar respostas
x1 = data.frame(prop = (prop.table(table(bls_2013$visitas2)))*100)
x2 = data.frame(prop = (prop.table(table(bls_2013$pedidos2)))*100)
x3 = data.frame(prop = (prop.table(table(bls_2013$destpp2)))*100)
x4 = data.frame(prop = (prop.table(table(bls_2013$pork2)))*100)
x5 = data.frame(prop = (prop.table(table(bls_2013$destcn2)))*100)
x6 = data.frame(prop = (prop.table(table(bls_2013$entidad2)))*100)
x7 = data.frame(prop = (prop.table(table(bls_2013$local2)))*100)
x8 = data.frame(prop = (prop.table(table(bls_2013$tvradio)))*100)
x9 = data.frame(prop = (prop.table(table(bls_2013$discplen)))*100)
x10 = data.frame(prop = (prop.table(table(bls_2013$votplen)))*100)

#incluir variavel com nome das alternativas
x1 = mutate(x1, atvs= "Realizar visitas às bases e manter contato direto com eleitores")
x2 = mutate(x2, atvs= "Atender aos pedidos de eleitores")
x3 = mutate(x3, atvs= "Ocupar posições de destaque na estrutura partidária")
x4 = mutate(x4, atvs= "Apresentar emendas ao orçamento e garantir a sua execução")
x5 = mutate(x5, atvs= "Ocupar posições de destaque no Congresso")
x6 = mutate(x6, atvs= "Manter relações com entidades da sociedade civil")
x7 = mutate(x7, atvs= "Encaminhar demandas dos prefeitos ou lideranças locais")
x8 = mutate(x8, atvs= "Manter visibilidade midiática nas TVs e rádios Câmara/Senado")
x9 = mutate(x9, atvs= "Proferir discursos em plenário")
x10 =mutate(x10, atvs= "Participar de votações em plenário")

#combinar
ativ_parl = rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

#renomear variavel - não funcionou
ativ_parl %>% 
  rename(resp = prop.Var1)

#Substituir codigos por labels
revalue(ativ_parl$prop.Var1, c("-999" = "Não respondeu")) -> ativ_parl$prop.Var1
revalue(ativ_parl$prop.Var1, c("1" = "Nenhuma importância")) -> ativ_parl$prop.Var1
revalue(ativ_parl$prop.Var1, c("5" = "Essencial")) -> ativ_parl$prop.Var1

#ordenar escala
ativ_parl$prop.Var1 = ordered(ativ_parl$prop.Var1)
ativ_parl$prop.Var1 = factor(
  ativ_parl$prop.Var1,levels = c("Não respondeu","Nenhuma importância","2","3","4","Essencial"))

#inverter escala para resolver a loc dos rotulos
escala_inversa = c("Essencial", "4", "3", "2", "Nenhuma importância", "Não respondeu")

ativ_parl$prop.Var1 = factor(ativ_parl$prop.Var1, escala_inversa)

#reduzir decimais do %
ativ_parl$prop.Freq = round(ativ_parl$prop.Freq, 2)

#grafico
g1 = ggplot(ativ_parl, aes(x=atvs,y=prop.Freq,fill=prop.Var1, label=prop.Freq, (x = reorder(prop.Freq, desc(prop.Freq)))))+
  geom_bar(stat = "identity", position = "fill")

g1 +scale_fill_brewer(palette = "Reds",  direction=-1)+
  theme_minimal()+
  coord_flip()+
  labs(x = "",
       y = "% de resposta",
       fill = "Grau de importância",
       title = "Importância das seguintes atividades para o futuro eleitoral de um congressista",
       subtitle = "Brazilian Legislative Survey, ano 2013",
       caption = "Fonte: Elaboração própria")+
  geom_label(aes(label = prop.Freq),
                    position = position_fill(vjust = 0.5),
                    size = 2.75,
                    colour = 'gray12',
                    fill = 'white')

#g1 = ggplotly(g1)
