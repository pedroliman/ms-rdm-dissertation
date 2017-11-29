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

opcoes = list(
  VarResposta = "Cash",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 100,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)


## Carregando Modelo
source('modelo-calibracao.R', encoding = 'UTF-8')

## Carregando objetos da Calibracao
source(file = "calibracao.R", encoding = "UTF-8")

source(file = "app.R", encoding = "UTF-8")

## Gerar Capitulo 4:
rmarkdown::render(input = "Capitulo-4.Rmd")


# Simulando uma vez apenas
resultado_unico <- data.frame(ode(y=stocks, times=simtime, func = modelo, 
                  parms=auxs, method="euler"))




## GE




resultado_transposto = t(resultado_unico)

write.csv2(x = resultado_transposto, file = "resultadosdoR.csv")


results = simularRDM_e_escolher_estrategia(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes)

incertezas = c("aAdvertisingEffectiveness", "aContactRate", "aAdoptionFraction", "aAdvertisingCost", "aAverageTicket")

ensemble_analisado = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble, dados_regret = results$AnaliseRegret$Dados, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias, var_resposta = opcoes$VarResposta, estrategia_candidata = results$EstrategiaCandidata)

plot_estrategias_versus_incertezas(ensemble_analisado, incertezas)













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
