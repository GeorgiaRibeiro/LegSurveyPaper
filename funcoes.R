#--------------------------------------------------#
#     Artigo Political Methods Uruguai Nov/2019    #
#       Georgia Ribeiro Git: @georgiaribeiro       #
#                ~ Análise MEPs                    # 
#--------------------------------------------------#

#------------ gaveta de funções -----------#

themeu = function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 1),
          plot.subtitle = element_text(hjust =1),
          text = element_text(size = 10),
          axis.title = element_text(face="bold"),
          legend.position = "bottom",
          axis.text.x=element_text(size = 9 )) +
    scale_fill_brewer(palette = "Set2")
}
