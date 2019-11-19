#-------------------------------------------#
# Artigo Political Methods Uruguai Nov/2019 #
#   Georgia Ribeiro Git: @georgiaribeiro    #
#                Análise BLS                #
#-------------------------------------------#

#---------------Resumo: cod variáveis--------------#
# wave: ano da aplicação do survey
# Atividades: visitas2, pedidos2, destpp2, pork2,
#            destcn2, entidad2, local2, tvradio,
#            discplen, votplen
# -------------------------------------------------#

#carregar pacotes
library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)
library(sqldf)
library(rlang)

#Banco e variáveis
bls = load("BLS8.RData")

#Filtrar por onda do survey
bls_2013 = filter(bls, wave == 2013)

#agrupar atividades em df
bls_atvs = data.frame(visitas = bls_2013$visitas2, pedidos = bls_2013$pedidos2,
                      destpp = bls_2013$destpp2, pork = bls_2013$pork2,
                      destcn = bls_2013$destcn2, entidad = bls_2013$entidad2,
                      tvradio = bls_2013$tvradio, discplen = bls_2013$discplen,
                      votplen = bls_2013$votplen)

#definir variaveis como factor
bls_atvs[]=lapply(bls_atvs, factor)

# ---------- ATIVIDADES PARLAMENTARES -----------#
#somar respostas
x1 = data.frame(prop = (prop.table(table(bls_atvs$visitas)))*100)
x2 = data.frame(prop = (prop.table(table(bls_atvs$pedidos)))*100)
x3 = data.frame(prop = (prop.table(table(bls_atvs$destpp)))*100)
x4 = data.frame(prop = (prop.table(table(bls_atvs$pork)))*100)
x5 = data.frame(prop = (prop.table(table(bls_atvs$destcn)))*100)
x6 = data.frame(prop = (prop.table(table(bls_atvs$entidad)))*100)
x7 = data.frame(prop = (prop.table(table(bls_atvs$local)))*100)
x8 = data.frame(prop = (prop.table(table(bls_atvs$tvradio)))*100)
x9 = data.frame(prop = (prop.table(table(bls_atvs$discplen)))*100)
x10 = data.frame(prop = (prop.table(table(bls_atvs$votplen)))*100)

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
#renomear colunas
colnames(ativ_parl) = c('escala', 'perc', 'atvs')

#Substituir codigos por labels
revalue(ativ_parl$escala, c("-999" = "Não respondeu")) -> ativ_parl$escala
revalue(ativ_parl$escala, c("1" = "Nenhuma importância")) -> ativ_parl$escala
revalue(ativ_parl$escala, c("5" = "Essencial")) -> ativ_parl$escala

#ordenar escala
ativ_parl$escala = ordered(ativ_parl$escala)
ativ_parl$escala = factor(
  ativ_parl$escala,levels = c("Não respondeu","Nenhuma importância","2","3","4","Essencial"))

#inverter escala para resolver a loc dos rotulos
ativ_parl$escala = factor(ativ_parl$escala, 
                             c("Essencial", "4", "3", "2", "Nenhuma importância", "Não respondeu"))
levels(ativ_parl$escala)

#ordenar por mais essencial
ativs2 = expand.grid(escala =unique(ativ_parl$escala),
                     atvs=unique(ativ_parl$atvs)) %>%
  data.frame %>% left_join(ativ_parl)

ordem = ativs2 %>% filter(escala=="Essencial") %>%
        arrange(desc(perc)) %>% .$atvs %>% as.character

#---------- grafico ----------#
g_ativs = ggplot(ativ_parl, aes(x=atvs,y=round(perc,2),fill=escala,
                                label=perc))+
  geom_bar(stat = "identity", position = "fill") +
  scale_x_discrete(limits=ordem)+
  scale_fill_brewer(palette = "Reds",  direction=-1)+
  coord_flip()+
  labs(x = "", y = " ", fill = "Grau de importância",
       title = "Importância das atividades para o futuro eleitoral de um congressista",
       subtitle = "Brazilian Legislative Survey, ano 2013",
       caption = "Fonte: Elaboração própria")+
  geom_label(aes(label = round(perc,0)),
             position = position_fill(vjust = 0.5),
             size = 3, colour = 'gray12', fill = 'white')+
  theme_minimal()+
  theme(axis.title.x = element_text(size = 9))
print(g_ativs)

g_ativs + ggsave("ativs_bls.png",
                   path = "graficos",
                   width = 7, height = 4, units = "in")

