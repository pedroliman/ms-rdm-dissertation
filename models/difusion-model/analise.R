###############################################################
# Autor: Pedro Nascimento de Lima, 2017
# Código fonte desenvolvido para a Dissertação de Mestrado.
# Arquivo: analise.R
# Objetivo: Este arquivo contém funções utilizadas para as análises 
# RDM realizadas durante a dissertação.
###############################################################

library(akima)
# library(prim)
library(dplyr)


# Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

## Carregando o Modelo, e outros objetos
source('modelo-difusao.R', encoding = 'UTF-8')

opcoes = list(
  VarResposta = "Cash",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 500,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

results = simularRDM_e_escolher_estrategia(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes)


ensemble_analisado = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble, dados_regret = results$AnaliseRegret$Dados, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias, var_resposta = opcoes$VarResposta)

analisar_ensemble_com_melhor_estrategia = function(ensemble, dados_regret, var_cenarios, var_estrategias, var_resposta) {
  
  ensemble = as.data.frame(ensemble)
  dados_regret = as.data.frame(dados_regret)
  
  
  dados_regret["MelhorEstrategia"] = dados_regret[var_resposta] == dados_regret$MaximoPorScenario
  
  linhas_melhores_estrategias = which(dados_regret[var_resposta] == dados_regret$MaximoPorScenario)
  
  variaveis = c(var_cenarios, var_estrategias, var_resposta)
  
  melhores_estrategias = as.data.frame(dados_regret[linhas_melhores_estrategias, variaveis])
  
  
  ensemble_com_melhor_estrategia = dplyr::inner_join(ensemble, melhores_estrategias)
  
  ensemble_com_melhor_estrategia
}


ensemble_analisado$Lever = as.factor(ensemble_analisado$Lever)

incertezas = c("aAdvertisingEffectiveness", "aContactRate", "aAdoptionFraction", "aAdvertisingCost", "aAverageTicket")

p = GGally::ggpairs(ensemble_analisado, columns = incertezas, aes(colour = Lever, alpha = 0.7))

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=rainbow(9)) +
      scale_color_manual(values=rainbow(9))  
  }
}

p




lever = results$EstrategiaCandidata
variaveis = c("CashRegretPerc","CashRegret","Cash", "AverageTicket", "AdvertisingCost", "AdvEffectiveness", "ContactRate", "AdoptionFraction")

sdrdm.pairs_plot(data = results$AnaliseRegret$Dados, lever = 1, variables = variaveis)


grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "Cash")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "Adopters")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "CashRegretPerc")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "CashRegret")








# Considerando o último ano

# + guides(color=FALSE) +

# + scale_color_manual(values = Mypalette(5))

# # Fazer Depois: Considerar Estratégias
# # expand.grid(ensemble, as.matrix(inputs$Levers))
# 
# # A biblioteca plotly entra em conflito com uma função usada pelo OpenMORDM!
# library(OpenMORDM)
# assign("mordm.globals", new.env())
# # Tentando fazer scenario discovery
# # assign("mordm.globals", new.env(), envir=parent.env(environment()))
# 
# 
# dados_sd = dplyr::filter(dados_simulacao, Tempo == FINISH) 
# 
# # Variáveis a considerar na análise
# vfatores = c("AdvEffectiveness", "ContactRate", "AdoptionFraction")
# factors = dados_sd[,vfatores]
# 
# vresposta = c("Adopters")
# response = dados_sd[,vresposta]
# # response = (dados_sd[,2] > 15000) * 1
# thr = 987470
# thr.type = -1 # >= x
# 
# # Esta linha de código agora funciona:
# analise_prim = analyze.prim(factors, response, threshold = thr, threshold.type = thr.type, which.box = 1)
# 
# # Aqui abaixo há
# box_prim = prim::prim.box(x = factors, y = response, threshold = thr, threshold.type = thr.type)
# 
# # Até aqui não consegui 
# 
# 
# # Analisar com o PRIM / MORDM.
# library(prim)
# dados_prim = dados_simulacao # dplyr::filter(dados_simulacao, Tempo == FINISH)
# factors = as.matrix(dados_prim[,c(4:11,1)])
# response = as.matrix(dados_prim[3])
# 
# analyze.prim(factors, response, threshold.type=-1,
#              threshold=10661)
# 
# mordm.correlation(factors, ht = 0.75, lt = 0.25, all = FALSE)
# 
# box = prim.box(x = factors, y = response, threshold = 10661, threshold.type = -1)
# 
# 
# 
# 
# # # Visualizando os Resultados
# 
# dados = dados_simulacao
# x_axis_name = "Tempo"
# y_axis_name = "Adopters"
# 
# 
# 
# 
# 
# # Não funcionou o gráfico por Leveler (por algum problema ainda não identificado, e não são os Nas e Infinitos.)
# 
# 
# 
# 
# 
# 
# 
# # Visualizando Todas as Replicações
# ggplot(dados_simulacao,
#        aes(x=Tempo, y=Adopters, color=Replicacao, group=Replicacao)) + 
#   geom_line() + 
#   ylab("Adopters") + 
#   xlab("Tempo") + guides(color=FALSE)
# 
# 
# # 




# Armazenando os Resultados
# write.csv(dados_simulacao, file = "dados_simulados_difusao.csv", row.names = FALSE)
