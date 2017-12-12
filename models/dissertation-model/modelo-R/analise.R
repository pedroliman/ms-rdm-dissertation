# Autor: Pedro Nascimento de Lima, 2017
# Código fonte desenvolvido para a Dissertação de Mestrado.
# Arquivo: analise.R
# Objetivo: Este arquivo contém funções utilizadas para as análises 
# RDM realizadas durante a dissertação.

#### Início: Carregando Bibliotecas e Funções ####

library(deSolve)
library(ggplot2)
library(gdata)
library(scales)
library(FME)
library(readxl)
library(akima)
# library(prim)
library(dplyr)

# Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

source(file = "demonstracoes.R", encoding = "UTF-8")

opcoes = list(
  VarResposta = "sNPVProfit1",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 20,
  VarTempo = "time",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

# Parâmetros para a Geração dos Gráficos
plots_width = 6
plots_heigh = 3


#### Replicando Sterman ####
## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
START<-0; FINISH<-40; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.00001; BROWSE_ON_DIFF = FALSE

## Carregando Modelo
source('modelo-calibracao.R', encoding = 'UTF-8')

# Rodando a Simulação com os Parâmetros do Sterman, rodando uma vez apenas.
arquivo_parametros = "./analise-sterman/params.xlsx"

parametros_completos = readxl::read_xlsx(arquivo_parametros, sheet = "params")

parametros_sterman = t(parametros_completos[,"Sterman"])[1,]

names(parametros_sterman) = as.matrix(parametros_completos[,1])

# Mudando o tempo de simulação para Simular o Sterman
resultados_sterman = solve_modelo_dissertacao(parametros = parametros_sterman, modelo = sdmodel$Modelo, simtime = sdmodel$SimTime)

# Gerando Gráficos
sterman_plots = list(
  grafico_npv_sterman = plot_linha_uma_variavel(dados = resultados_sterman, variavel = "sNPVProfit1", nome_amigavel_variavel = "Valor Presente Liquido"),
  
  grafico_preco_sterman = plot_linha_uma_variavel(dados = resultados_sterman, variavel = "sPrice1", nome_amigavel_variavel = "Preço Player 1"),
  
  grafico_demanda_sterman = plot_linha_uma_variavel(dados = resultados_sterman, variavel = "fIndustryOrderRate", nome_amigavel_variavel = "Demanda Anual Total"),
  
  grafico_vpl_preco = plot_linha_duas_variaveis(dados = resultados_sterman, variavel1 = "sNPVProfit1", variavel2 = "sPrice1", nome_amigavel_variavel1 = "VPL", nome_amigavel_variavel2 = "Preço"),
  
  grafico_vpl_demanda = plot_linha_duas_variaveis(dados = resultados_sterman, variavel1 = "sNPVProfit1", variavel2 = "fIndustryOrderRate", nome_amigavel_variavel1 = "VPL", nome_amigavel_variavel2 = "Demanda Global")
  
)

# Salvando Resultados do Sterman e Plots (para eventual verificação posterior):
save(sdmodel, parametros_sterman, resultados_sterman, sterman_plots, file = "./analise-sterman/resultados_sterman.Rdata")
# Para carregar depois, basta rodar:
# load(file = "./analise-sterman/resultados_sterman.Rdata")

# Salvando todos os Gráficos do Sterman:
mapply(ggsave, file=paste0("./images/", names(sterman_plots), ".png"), plot=sterman_plots, width = plots_width, height = plots_heigh)



#### RODADA 1 ####

## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
START<-0; FINISH<-10; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.00001; BROWSE_ON_DIFF = FALSE

# Carregando o Modelo Novamente:
source('modelo-calibracao.R', encoding = 'UTF-8')

results = simularRDM_e_escolher_estrategia(inputs = "./rodada1/params.xlsx", sdmodel = sdmodel, opcoes = opcoes)
# Salvando Resultados
save(resultados_sterman, results)

# Gráficos

plot_estrategia1 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = 1)

plot_estrategia1e10 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = c(6, 7))
plot_preco_estrategia10 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sPrice1", nome_amigavel_variavel = "Preço", estrategia = c(3))


plot_estrategia_candidata = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = results$EstrategiaCandidata)

plot_whisker_lever_perc_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1RegretPerc")

plot_whisker_lever_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1Regret")

plot_whisker_lever_profit = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1")

# Verificar resultados com NA

View(results$DadosSimulados[which(results$DadosSimulados$Scenario == 1),])



### Analisar ENsemble

ensemble_analisado = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble,
                                                             dados_regret = results$AnaliseRegret$Dados, 
                                                             var_cenarios = opcoes$VarCenarios, 
                                                             var_estrategias = opcoes$VarEstrategias, 
                                                             var_resposta = opcoes$VarResposta, 
                                                             estrategia_candidata = results$EstrategiaCandidata)


parametros_lidos = readxl::read_xlsx("params.xlsx", sheet = "params")

incertezas = parametros_lidos[which(!(parametros_lidos[,"Min"]==parametros_lidos[,"Max"])),"Variavel"][[1]]

incertezas = c("aUnitsPerHousehold", "aFractionalDiscardRate", "aReferenceIndustryDemandElasticity", "aVolumeReportingDelay")


# Ajuda a Identificar em Que condições a Estratégia Candidata não é a melhor.
plot_estrategias_incertezas = plot_estrategias_versus_incertezas(ensemble_analisado = ensemble_analisado,incertezas = incertezas)


#### RODADA 2 ####

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
