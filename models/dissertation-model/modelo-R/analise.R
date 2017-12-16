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
  N = 500,
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


View(results$DadosSimulados$Scenario)

dplyr::group_by(results$DadosSimulados, Scenario) %>% dplyr::summarise(MaxTime = max(time))


results$DadosUltimoPeriodo

# Salvando Resultados
# save(results, file = "./rodada1/resultados.Rdata")
# Gráficos

plots_rodada1 = list(
  plot_estrategia1 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = 1),
  plot_1_e_candidata = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = c(1, results$EstrategiaCandidata)),
  plot_preco_estrategia10 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sPrice1", nome_amigavel_variavel = "Preço", estrategia = c(3)),
  plot_estrategia_candidata = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = results$EstrategiaCandidata),
  plot_whisker_lever_perc_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1RegretPerc"),
  plot_whisker_lever_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1Regret"),
  plot_whisker_lever_profit = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1")
)



# Verificar resultados com NA

# Salvar Plots em Imagem:
mapply(ggsave, file=paste0("./images/", names(plots_rodada1), ".png"), plot=plots_rodada1, width = plots_width, height = plots_heigh)


### Analisar ENsemble

ensemble_analisado = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble,
                                                             dados_regret = results$AnaliseRegret$Dados, 
                                                             var_cenarios = opcoes$VarCenarios, 
                                                             var_estrategias = opcoes$VarEstrategias, 
                                                             var_resposta = opcoes$VarResposta, 
                                                             estrategia_candidata = results$EstrategiaCandidata)


parametros_lidos = readxl::read_xlsx("./rodada1/params.xlsx", sheet = "params")

incertezas = parametros_lidos[which(!(parametros_lidos[,"Min"]==parametros_lidos[,"Max"])),"Variavel"][[1]]

incertezas = c("aLCStrength", "aNormalCapacityUtilization", "aSensOfAttractToPrice", "aRatioOfFixedToVarCost")


# Ajuda a Identificar em Que condições a Estratégia Candidata não é a melhor.
plot_estrategias_incertezas = plot_estrategias_versus_incertezas(ensemble_analisado = ensemble_analisado,incertezas = incertezas)

ggsave(filename = "./images/plot_estrategias_incertezas.png", plot = plot_estrategias_incertezas, width = 10, height = 6)




### Rodadno Feature Selection para Saber as Incertezas mais relevantes (computacionalmente):
# https://www.linkedin.com/pulse/r-finding-most-important-predictor-variables-saranya-anandh/
# Gerando um ensemble ampliado para a estratégia 4:
estrategia = 2
ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")

ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia),]

variaveis_incertas = colnames(results$Ensemble)

variaveis_incertas = variaveis_incertas[which(variaveis_incertas %in% names(ensemble_e_resultados))]

variavel_resposta = "sNPV1ProfitRegret"

# Os dados do ultimo período não foram substituidos (por algum motivo que eu não sei qual é.)
class(ensemble_e_resultados)

ensemble_e_resultados$sNPVProfit1Regret


library(party)
cf1 <- cforest(ensemble_e_resultados$sNPVProfit1Regret ~ . , data= ensemble_e_resultados[,variaveis_incertas], control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
varimp(cf1) # get variable importance, based on mean decrease in accuracy#=>                 Month          Day_of_month           Day_of_week #=>           0.689167598           0.115937291          -0.004641633 #=>       pressure_height            Wind_speed              Humidity #=>           5.519633507           0.125868789           3.474611356 #=>  Temperature_Sandburg   Temperature_ElMonte Inversion_base_height #=>          12.878794481          14.175901506           4.276103121 #=>     Pressure_gradient Inversion_temperature            Visibility #=>           3.234732558          11.738969777           2.283430842
varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors#=>                 Month          Day_of_month           Day_of_week #=>            0.08899435            0.19311805            0.02526252 #=>       pressure_height            Wind_speed              Humidity #=>            0.35458493           -0.19089686            0.14617239 #=>  Temperature_Sandburg   Temperature_ElMonte Inversion_base_height #=>            0.74640367            1.19786882            0.69662788 #=>     Pressure_gradient Inversion_temperature            Visibility #=>            0.58295887            0.65507322            0.05380003
varimpAUC(cf1)  # more robust towards class imbalance.#=>                 Month          Day_of_month           Day_of_week #=>            1.12821259           -0.04079495            0.07800158 #=>       pressure_height            Wind_speed              Humidity #=>            5.85160593            0.11250973            3.32289714 #=>  Temperature_Sandburg   Temperature_ElMonte Inversion_base_height #=>           11.97425093           13.66085973            3.70572939 #=>     Pressure_gradient Inversion_temperature            Visibility #=>            3.05169171           11.48762432            2.04145930


library(relaimpo)
lmMod <- lm(ensemble_e_resultados$sNPVProfit1Regret ~ . , data = ensemble_e_resultados[,variaveis_incertas])  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)
calc.relimp(lmMod, rela = TRUE)
# calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # rel

# por algum motivo o discount Rate não está lá dentro:

library(breakDown)
br = broken(lmMod, new_observation = ensemble_analisado[18,])
plot(br)


### O resultado desta análise fez um pouco de sentido (e não é óbvio).
library(earth)
marsModel <- earth(ensemble_e_resultados$sNPVProfit1Regret ~ ., data=ensemble_e_resultados[,variaveis_incertas]) # build model
ev <- evimp(marsModel) # estimate variable importance#=>                       nsubsets   gcv    rss#=> Temperature_ElMonte         29 100.0  100.0#=> Pressure_gradient           28  42.5   48.4#=> pressure_height             26  30.1   38.1#=> Month9                      25  26.1   34.8#=> Month5                      24  21.9   31.7#=> Month4                      23  19.9   30.0#=> Month3                      22  17.6   28.3#=> Inversion_base_height       21  14.4   26.1#=> Month11                     19  12.3   24.1#=> Visibility                  18  11.4   23.2#=> Day_of_month23              14   8.9   19.8#=> Humidity                    13   7.4   18.7#=> Month6                      11   5.1   16.6#=> Temperature_Sandburg         9   7.0   15.6#=> Wind_speed                   7   5.1   13.4#=> Month12                      6   4.2   12.3#=> Day_of_month9                3   4.6    9.1#=> Day_of_week4                 2  -3.9    5.9#=> Day_of_month7-unused         1  -4.7    2.8
plot(ev)


# Stepwise (Também faz sentido)
base.mod <- lm(ensemble_e_resultados$sNPVProfit1Regret ~ 1 , data= ensemble_e_resultados[,variaveis_incertas])  # base intercept only model
all.mod <- lm(ensemble_e_resultados$sNPVProfit1Regret ~ . , data= ensemble_e_resultados[,variaveis_incertas]) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)

# Rodando o Algoritmo Boruta
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(ensemble_e_resultados$sNPVProfit1Regret ~ ., data=na.omit(ensemble_e_resultados[,variaveis_incertas]), doTrace=2)  # perform Boruta search# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables#=> [1] "Month"                 "ozone_reading"         "pressure_height"      #=> [4] "Humidity"              "Temperature_Sandburg"  "Temperature_ElMonte"  #=> [7] "Inversion_base_height" "Pressure_gradient"     "Inversion_temperature"#=> [10] "Visibility"
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance



landscape_estrategia1 = plot_landscape_futuros_plausiveis(
  results, estrategia = 2, 
  variavelresp = "sNPVProfit1Regret",
  nomeamigavel_variavelresp = "VPL",
  variavel1  = "aRatioOfFixedToVarCost",
  n_variavel1 = "Sensibilidade ao Preço",
  variavel2 = "aNormalCapacityUtilization",
  n_variavel2 = "Utilização da Capacidade"
)



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
