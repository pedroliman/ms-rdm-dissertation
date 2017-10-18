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
  N = 100,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

results = simularRDM_e_escolher_estrategia(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes)



gerar_grafico_regret_perc = function(dados_regret) {
  dados_por_estrategia = dplyr::group_by(dados_regret, Lever)
  
  dados_por_estrategia$Lever = as.factor(dados_por_estrategia$Lever)
  
  # Gerando Grafico da Variável de Perda de Oportunidade
  
  p <- ggplot(dados_por_estrategia, aes(y = CashRegretPerc,x = Lever, group = Lever))
  p + geom_boxplot()
}




grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "Cash")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "Adopters")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "CashRegretPerc")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "CashRegret")



View(results$AnaliseRegret$Dados)

p + geom_boxplot() + geom_jitter(width = 0.2)
p + geom_violin() + geom_jitter(height = 0, width = 0.1)
p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


# Gerando Grafico da Variável de Perda de Oportunidade Percentual
library(ggplot2)
p <- ggplot(dados_por_estrategia, aes(y = Cash_Percent_Regret,x = Lever, group = Lever))
p + geom_boxplot() + geom_jitter(width = 0.2)
p + geom_violin() + geom_jitter(height = 0, width = 0.1)
p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Gerando Grafico da Variável de Resposta                                      
library(ggplot2)
p <- ggplot(dados_por_estrategia, aes(y = Cash,x = Lever, group = Lever))
p + geom_boxplot()



library(ggplot2)
# Visualizando Todas as Replicações
ggplot2::ggplot(dados_simulacao,
       aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Clientes") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


# Plotando o Adoption Rate 
ggplot2::ggplot(dados_simulacao,
                aes(x=Tempo, y=Adoption_Rate, color=factor(Lever), group=Replicacao)) + 
  geom_line() + 
  ylab("Taxa de Adoção") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


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
