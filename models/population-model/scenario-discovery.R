# Tentando fazer scenario discovery

library(sdtoolkit)

dados_sd = dadosplot = dplyr::filter(dados_simulacao, Tempo == FINISH) 

write.csv(dados_sd, file = "dadossd.csv")
