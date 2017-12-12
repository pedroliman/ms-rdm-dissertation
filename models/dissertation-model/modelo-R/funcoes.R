###############################################################
# Autor: Pedro Nascimento de Lima, 2017
# Código fonte desenvolvido para a Dissertação de Mestrado.
# Arquivo: funcoes.R
# Objetivo: Este arquivo contém funções utilizadas para as análises 
# RDM realizadas durante a dissertação.
###############################################################

library(plotly)
library(lhs)
library(deSolve)
library(dplyr)
library(ggplot2)
library(GGally)

##### CONSTANTES #####
VAR_SCENARIO = "Scenario"
VAR_LEVER = "Lever"

##### SIMULAR RDM E ESCOLHER ESTRATEGIA #####

#' simularRDM_e_escolher_estrategia
#'
#' @param inputs caminho para o arquivo de inputs
#' @param sdmodel list com modelo e suas opções
#' @param opcoes list com opções para a simulação e analise de Regret.
#'
#' @return list com resultados da simulacao e uma estratégia candidata.
simularRDM_e_escolher_estrategia = function(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes) {
  
  
  output_simulacao = simular_RDM(arquivo_de_inputs=inputs ,sdmodel = sdmodel, n = opcoes$N)
  
  ## Simular
  dados_simulacao = output_simulacao$DadosSimulacao
  
  # Selecionando dados do último ano:
  dados = selecionar_ultimo_periodo(dados_simulacao = dados_simulacao, var_tempo = opcoes$VarTempo)
  
  # Analisar Regret
  analise_regret = calcular_e_resumir_regret(dados = dados, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  # Escolher a Estratégia Candidata, com base no critério de robustez dos percentis
  estrategia_candidata = escolher_estrategia_candidata(dados = analise_regret$Dados, resumo_estrategias = analise_regret$ResumoEstrategias, var_resposta = opcoes$VarResposta, var_criterio = opcoes$VarCriterio, sentido = opcoes$SentidoCriterio)
  
  message(paste("A Estrategia candidata é a ", estrategia_candidata$Lever))
  
  output = list(
    DadosSimulados = dados_simulacao,
    DadosUltimoPeriodo = dados,
    AnaliseRegret = analise_regret,
    Inputs = output_simulacao$Inputs,
    Ensemble = output_simulacao$Ensemble,
    EstrategiaCandidata =  as.numeric(estrategia_candidata[opcoes$VarEstrategias]),
    Opcoes = opcoes,
    SdModel = sdmodel
  )
  
  output
  
}


##### CARREGAR INPUTS #####

#' carregar_inputs
#'
#' @param arquivo_de_inputs caminho para o arquivo de inputs com estratégias e incertezas
#' @param abas_a_ler abas a ler do arquivo de inputs
#' @param nomes_inputs Nome a ser atribuido aos dataframes de input.
#'
#' @return list com inputs para a simulação.
carregar_inputs = function (arquivo_de_inputs="params.xlsx", abas_a_ler = c("params", "levers"), nomes_inputs = c("Parametros", "Levers")) {
  
  # Criando uma list para os inputs
  message(
    paste("01. funcoes.R/carregar_inputs: Iniciando Carregamento de Inputs (funcao carregar_inputs()",
          "arquivo_de_inputs = ", arquivo_de_inputs)
  )
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs
  
  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }
  
  message("01. funcoes.R/carregar_inputs: Finalizando Carregamento de Inputs.")
  return(inputs)
  
}


##### OBTER ENSEMBLE - PARÂMETROS #####

#' obter_lhs_ensemble
#'
#' @param params dataframe de parâmetros a usar (no padrão pré-determinado)
#' @param n tamanho do ensemble a montar
#'
#' @return dataframe com ensemble montado (pronto para a simulação)
obter_lhs_ensemble = function (params, n=100) {
  message("01. funcoes.R/obter_lhs_ensemble: Iniciando Obtenção do Ensemble.")
  #Obtendo DataFrame de Parâmetros
  
  nvar = length(params$Variavel)
  pontos = n
  
  # Obtendo um Hypercubo com as Variáveis que eu quero
  randomLHS <- randomLHS(pontos, nvar)
  
  p = as.data.frame(randomLHS)
  min = as.vector(params$Min)
  max = as.vector(params$Max)
  variaveis = as.vector(params$Variavel)
  
  # Transformando o Hypercubo em variáveis
  # var <- matrix(nrow=pontos, ncol=variaveis)
  ensemble = matrix(nrow = pontos, ncol = nvar+1)
  
  # Montando o Ensemble
  for (var in variaveis) {
    i = which(x = variaveis == var)
    
    # Aqui o i é +1 porque a primeira coluna será o cenário.
    ensemble[,i+1] = qunif(p = randomLHS[,i], min = min[i], max = max[i])
  }
  
  # Adicionando A variável "Scenario"
  variaveis = c(c(VAR_SCENARIO),variaveis)
  
  colnames(ensemble) = variaveis
  
  ensemble[,VAR_SCENARIO] = 1:nrow(ensemble)
  
  ensemble
}

##### AMPLIAR ENSEMBLE COM ESTRATÉGIAS #####

#' ampliar_ensemble_com_levers
#'
#' @param ensemble conjunto de cenarios a simular
#' @param levers conjunto de estratégias a simular
#'
#' @return dataframe com a combinação de todas as estratégias em todos os cenários.
ampliar_ensemble_com_levers = function(ensemble, levers) {
  
  variaveis_adicionais = names(dplyr::select(levers, -LeverCode))
  
  linhas_ensemble_incial = nrow(ensemble)
  novo_ensemble = matrix(0, nrow = nrow(ensemble)*length(levers$Lever), ncol = ncol(ensemble) + length(variaveis_adicionais))
  
  names_old_ensemble = colnames(ensemble)
  names_novo_ensemble = c(names_old_ensemble, variaveis_adicionais)
  
  colnames(novo_ensemble) = names_novo_ensemble
  
  j = 1
  for (l in seq_along(levers$Lever)) {
    lini = j
    lfim = j + linhas_ensemble_incial-1
    matriz_var_adicionais = as.matrix(levers[l,variaveis_adicionais])
    novo_ensemble[lini:lfim,names_old_ensemble] = ensemble
    novo_ensemble[lini:lfim,variaveis_adicionais] = matrix(matriz_var_adicionais, nrow = linhas_ensemble_incial, ncol = ncol(matriz_var_adicionais), byrow = TRUE)
    j = j + linhas_ensemble_incial
  }
  
  novo_ensemble
  
}


##### SIMULAR #####

#' simular
#'
#' @param stocks integrais a serem resolvidas numéricamente. (numeric) 
#' @param simtime Tempo de simulação (numeric)
#' @param modelo Modelo de dinâmica de sistemas no padrão do deSolve (function)
#' @param ensemble ensemble montado (pronto para a simulação)
#' @param nomes_variaveis_final vetor com nomes de variáveis
#'
#' @return
#' @export
#'
#' @examples
simular = function(simtime, modelo, ensemble, nomes_variaveis_final) {
  message("01. funcoes.R/simular: Iniciando Simulação.")
  # Rodando a Simulação (uma vez), com a primeira linha do ensemble - Ajuda a saber se funciona.
  # Esta função apenas funciona com o estoque inicial fixo, será necessário implementar de outra forma depois.
  
  o = as.data.frame(solve_modelo_dissertacao(parametros = ensemble[1,], modelo = modelo, simtime = simtime)) 
  
  nomes_temporario = names(o)
  
  # o<-data.frame(ode(y=stocks, times=simtime, func = modelo, 
  #                   parms=ensemble[1,], method="euler"))
  pontos = nrow(ensemble)
  
  nlinhas = nrow(o)
  
  ncolunas = ncol(o)+2
  
  # Montando uma matriz com todos os dados para a simulação
  dados_simulacao = matrix(nrow = pontos*nlinhas, ncol = ncolunas)
  
  # J é o índice dos dados simulados
  j = 1
  # Rodando a Simulacao Em todo o Ensemble
  
  for (i in 1:nrow(ensemble)) {
    
    #resultados_simulacao = ode(y=stocks, times=simtime, func = modelo, 
    #                           parms=ensemble[i,], method="euler")
    
    resultados_simulacao = as.data.frame(solve_modelo_dissertacao(parametros = ensemble[i,], modelo = modelo, simtime = simtime)) 
    
    resultados_simulacao = as.matrix(resultados_simulacao)
    
    linhas = nrow(resultados_simulacao)
    
    # Avançando a linha inicial e Final da Simulação
    l_inicial = j
    l_final = j + linhas-1
    
    # Adicionando o resultado ao ensemble
    dados_simulacao[l_inicial:l_final,1:(ncolunas-2)] = resultados_simulacao
    
    # Adicionando o Número do Cenário
    dados_simulacao[l_inicial:l_final,(ncolunas-1)] = ensemble[i,VAR_LEVER]
    
    # Adicionando o Número do Cenário
    dados_simulacao[l_inicial:l_final,ncolunas] = ensemble[i,VAR_SCENARIO]
    
    
    # Exibindo uma Mensagem de Status
    if (i %% 5 == 0) {
      message(paste(i, "simulações finalizadas."))
    }
    
    # Avançando o índice dos dados simulados
    j = j + linhas
  }
  
  # Usando nomes temporario
  colnames(dados_simulacao) = c(nomes_temporario, VAR_LEVER, VAR_SCENARIO)
  # colnames(dados_simulacao) = nomes_variaveis_final
  
  dados_simulacao = as.data.frame(dados_simulacao)
  names(dados_simulacao) = c(nomes_temporario, VAR_LEVER, VAR_SCENARIO)
  #names(dados_simulacao) = nomes_variaveis_final
  
  message("01. funcoes.R/simular: Finalizando Simulacao.")
  
  dados_simulacao
}


##### SIMULAR RDM #####

#' simular_RDM
#'
#' @param arquivo_de_inputs Caminho para o arquivo de dados padronizado com Estrategias e Incertezas (character)
#' @param sdmodel Lista com variáveis para simulação de dinamica de sistemas
#' @param n Número de cenarios a gerar (numeric)
#'
#' @return data.frame com resultados da simulação
simular_RDM = function(arquivo_de_inputs="params.xlsx", sdmodel, n = 10){
  t_inicio = Sys.time()
  message("Bem vindo ao SIMULADOR RDM! Pedro Lima.")
  message(paste("Iniciando Simulacao RDM: ", t_inicio))
  
  
  
  # Carregando Inputs
  inputs = carregar_inputs(arquivo_de_inputs = arquivo_de_inputs)
  
  # Obter Ensemble LHS (Sem Variáveis das Estratégias)
  ensemble = obter_lhs_ensemble(params = inputs$Parametros, n = n)
  
  # Ampliar Ensemble com as variáveis das Estratégias
  novo_ensemble = ampliar_ensemble_com_levers(ensemble = ensemble, levers = inputs$Levers)
  
  # Rodando a Simulação
  nestrategias = length(inputs$Levers$Lever)
  nfuturos = nrow(ensemble)
  ntempo = ((sdmodel$Finish - sdmodel$Start)/sdmodel$Step)
  
  message(paste("Esta rotina realizará", nestrategias * nfuturos, "Simulacoes.\n (", nestrategias, "estratégias x", nfuturos, "futuros, em", ntempo , "periodos de tempo."))
  
  # TODO: Esta Chamada vai precisar mudar para considerar a nova funcao
  dados_simulacao = simular(simtime = sdmodel$SimTime, modelo = sdmodel$Modelo, ensemble = novo_ensemble, nomes_variaveis_final = sdmodel$Variaveis)
  
  t_fim = Sys.time()
  
  message("Finalizando Simulacao. Tempo de Simulacao: ", t_fim - t_inicio)
  
  output = list(
    Inputs = inputs,
    Ensemble = ensemble,
    NovoEnsemble = novo_ensemble,
    DadosSimulacao = dados_simulacao
  )
  
  output
  
}


##### CALCULO DO REGRET (PERDA DE OPORTUNIDADE) #####

#' calcular_regret
#'
#' @param dados dataframe de dados simulados para o calculo do Regret.
#' @param var_resposta variável de resposta a utilizar no calculo de regret (quanto mais, melhor)
#' @param var_group variável a agrupar (ex.: Cenários)
#'
#' @return mesmo dataframe de entrada com variáveis a mais.
calcular_regret = function(dados, var_resposta, var_group) {
  var_maximo = paste("MaximoPor", var_group, sep = "")
  var_minimo = paste("MinimoPor", var_group, sep = "")
  var_regret = paste(var_resposta, "Regret", sep = "")
  var_regret_perc = paste(var_regret, "Perc", sep = "")
  
  dados[var_maximo] = calcular_maximo_por_variavel(var_resposta = var_resposta, var_group = var_group, dados = dados)
  
  dados[var_minimo] = calcular_minimo_por_variavel(var_resposta = var_resposta, var_group = var_group, dados = dados)
  
  dados[var_regret] = dados[var_maximo] - dados[var_resposta]
  
  dados[var_regret_perc] = dados[var_regret] / (dados[var_maximo] - dados[var_minimo])
  
  dados  
}


##### RESUMIR VARIÁVEL DE RESPOSTA PARA A ANÁLISE DO REGRET #####
#' resumir_variavel_resposta
#'
#' @param dados dataframe com dados para analise do regret.
#' @param var_resposta variável de resposta para análise do RDM.
#' @param var_group 
#'
#' @return dataframe com resumo das variaveis por grupo definido.
resumir_variavel_resposta = function(dados = dados_ano_final, var_resposta = "Cash", var_group = "Lever") {
  var_regret = paste(var_resposta, "Regret", sep = "")
  var_regret_perc = paste(var_regret, "Perc", sep = "")
  
  call = substitute(
    expr =
      dplyr::group_by(dados, VarGroup) 
    %>% select(VarGroup, VarResposta, VarRegret, VarRegretPerc)
    %>% summarise(VarMedio = mean(VarResposta),
                  VarDev = sd(VarResposta),
                  Percentil25Var = quantile(VarResposta, probs = c(0.25)),
                  Percentil75Var = quantile(VarResposta, probs = c(0.75)),
                  RegretMedio = mean(VarRegret),
                  DesvioRegret = sd(VarRegret),
                  Percentil25Regret = quantile(VarRegret, probs = c(0.25)),
                  Percentil75Regret = quantile(VarRegret, probs = c(0.75)),
                  RegretMedioPerc = mean(VarRegretPerc),
                  DesvioRegretPerc = sd(VarRegretPerc),
                  Percentil25RegretPerc = quantile(VarRegretPerc, probs = c(0.25)),
                  Percentil75RegretPerc = quantile(VarRegretPerc, probs = c(0.75))
    )
    ,
    env = list(VarGroup = as.name(var_group),
               VarResposta = as.name(var_resposta),
               VarRegret = as.name(var_regret),
               VarRegretPerc = as.name(var_regret_perc)
    )
  )
  
  resumo = eval(call)  
  
  colnames(resumo) = c(
    var_group,
    paste(var_resposta, "Medio", sep = ""),
    paste(var_resposta, "Desvio", sep = ""),
    paste(var_resposta, "Percentil25", sep = ""),
    paste(var_resposta, "Percentil75", sep = ""),
    paste(var_regret, "Medio", sep = ""),
    paste(var_regret, "Desvio", sep = ""),
    paste(var_regret, "Percentil25", sep = ""),
    paste(var_regret, "Percentil75", sep = ""),
    paste(var_regret_perc, "Medio", sep = ""),
    paste(var_regret_perc, "Desvio", sep = ""),
    paste(var_regret_perc, "Percentil25", sep = ""),
    paste(var_regret_perc, "Percentil75", sep = "")
  )
  
  resumo
}


##### ESCOLHER ESTRATÉGIA CANDIDATA #####

escolher_estrategia_candidata = function(dados, resumo_estrategias, var_resposta, var_criterio = "RegretPercPercentil75", sentido = "min") {
  
  var_respota_criterio = paste(var_resposta, var_criterio, sep = "")
  
  
  # Esta lista de criterios deve ser mantida igual à lista que a funcao resumir_variavel_resposta()
  possiveis_var_criterios = c("Percentil25", "Percentil75", "Medio", "Desvio", "RegretMedio", "RegretDesvio", "RegretPercentil25", "RegretPercentil75", "RegretPercMedio", "RegretPercDesvio", "RegretPercPercentil25", "RegretPercPercentil75")
  
  # Conferindo alguns pressupostos basicos:
  possiveis_var_respota_e_criterios = paste(var_resposta, possiveis_var_criterios, sep = "")
  
  # Conferindo se a variável de resposta e variável de critério combinam corretamente:
  if (!all(possiveis_var_respota_e_criterios %in% names(resumo_estrategias))){
    stop("Existe algo errado com a sua variavel de resposta ou variavel de criterio (a combinacao das duas no existe no resumo de estrategias).")
  }
  
  # Conferindo se a Variavel de criterio está correta.
  if(!var_criterio %in% possiveis_var_criterios){
    stop(paste("Esta variavel de criterio esta incorreta. escolha entre:",possiveis_var_criterios))
  }
  
  
  # Agora sim, posso escolhenr a estratégia que tem o menor percentil percentual 75 (assim como Lempert):
  estrategias_candidatas = switch(sentido,
                                  "min" = escolher_estrategia_min(resumo_estrategias, var_respota_criterio),
                                  "max" = escolher_estrategia_max(resumo_estrategias, var_respota_criterio))
  
  estrategias_candidatas
}


##### CALCULAR E RESUMIR REGRET #####

calcular_e_resumir_regret = function(dados, var_resposta, var_cenarios, var_estrategias) {
  dados = calcular_regret(dados = dados, var_resposta = var_resposta, var_group = var_cenarios)
  
  # Resumindo Variável de Resposta Cash:
  resumo_estrategias = resumir_variavel_resposta(dados = dados, var_resposta = var_resposta, var_group = var_estrategias)
  
  # Formar lista de outputs dessta análise
  output = list(
    Dados = dados,
    ResumoEstrategias = resumo_estrategias
  )
  
  output
}



##### ESCOLHER ESTRATÉGIA #####

escolher_estrategia_min = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == min(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]  
  estrategia
}


escolher_estrategia_max = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == max(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]  
  estrategia
}


##### ANALISAR ENSEMBLE DETERMINANDO A MELHOR ESTRATÉGIA #####
analisar_ensemble_com_melhor_estrategia = function(ensemble, dados_regret, var_cenarios, var_estrategias, var_resposta, estrategia_candidata) {
  
  
  ensemble = as.data.frame(ensemble)
  dados_regret = as.data.frame(dados_regret)
  
  
  dados_regret["MelhorEstrategia"] = dados_regret[var_resposta] == dados_regret$MaximoPorScenario
  
  linhas_melhores_estrategias = which(dados_regret[var_resposta] == dados_regret$MaximoPorScenario)
  
  variaveis = c(var_cenarios, var_estrategias, var_resposta)
  
  melhores_estrategias = as.data.frame(dados_regret[linhas_melhores_estrategias, variaveis])
  
  ensemble_com_melhor_estrategia = dplyr::inner_join(ensemble, melhores_estrategias)
  
  ensemble_com_melhor_estrategia["EstrategiaCandidata"] = ensemble_com_melhor_estrategia[var_estrategias] == estrategia_candidata
  
  #ensemble_com_melhor_estrategia = as.factor(ensemble_com_melhor_estrategia[var_estrategias])
  
  ensemble_com_melhor_estrategia
  
}


##### FUNÇÕES AUXILIARES #####

library(dplyr)
selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)  
}


selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)  
}


calcular_maximo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Maximo = max(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Maximo
}

calcular_minimo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Minimo = min(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Minimo
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

##### FUNÇÕES CRIADAS NA CALIBRAÇÃO ####



solve_modelo_dissertacao <- function(parametros, modelo, simtime){
  
  # Número de Players no modelo
  N_PLAYERS <<- 2
  
  # All the stocks are initialised here...
  
  n_tempo = length(simtime)
  
  list.variaveis.globais <<- list(
    sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
    aExpectedIndustryDemand = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo)
  )
  
  ##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
  auxs    <- list(aDiscountRate = unname(parametros["aDiscountRate"])
                  ,aNormalDeliveryDelay = rep(unname(parametros["aNormalDeliveryDelay"]), times = N_PLAYERS)
                  ,aSwitchForCapacity = unname(parametros["aSwitchForCapacity"])
                  # Vamos testar apenas um parâmetro por enquanto
                  ,aFractionalDiscardRate = unname(parametros["aFractionalDiscardRate"]) # unname(pars["aFractionalDiscardRate"]) # Original 0.1
                  ,aInitialDiffusionFraction = unname(parametros["aInitialDiffusionFraction"])
                  ,aReferencePrice = unname(parametros["aReferencePrice"])
                  ,aReferenceIndustryDemandElasticity = unname(parametros["aReferenceIndustryDemandElasticity"])
                  ,aReferencePopulation = unname(parametros["aReferencePopulation"])
                  ,aInnovatorAdoptionFraction = unname(parametros["aInnovatorAdoptionFraction"])
                  ,aWOMStrength = unname(parametros["aWOMStrength"]) # unname(pars["aWOMStrength"]) # Original 1
                  ,aPopulation = unname(parametros["aPopulation"]) #100000000 # Original Sterman: 100000000
                  ,aUnitsPerHousehold = unname(parametros["aUnitsPerHousehold"])
                  ,aSwitchForShipmentsInForecast = unname(parametros["aSwitchForShipmentsInForecast"])
                  ,aVolumeReportingDelay = rep(unname(parametros["aVolumeReportingDelay"]), times = N_PLAYERS)
                  ,aForecastHorizon = rep(unname(parametros["aForecastHorizon"]), times = N_PLAYERS)
                  ,aCapacityAcquisitionDelay = unname(parametros["aCapacityAcquisitionDelay"])
                  ,aTimeForHistoricalVolume = unname(parametros["aTimeForHistoricalVolume"])
                  # Market Sector
                  ,aReferenceDeliveryDelay = unname(parametros["aReferenceDeliveryDelay"])
                  ,aSensOfAttractToAvailability = unname(parametros["aSensOfAttractToAvailability"])
                  ,aSensOfAttractToPrice = unname(parametros["aSensOfAttractToPrice"])
                  # Learning Curve Params
                  ,aLCStrength = rep(unname(parametros["aLCStrength"]), times = N_PLAYERS)
                  ,aInitialProductionExperience = rep(unname(parametros["aInitialProductionExperience"]), times = N_PLAYERS)
                  ,aRatioOfFixedToVarCost = rep(unname(parametros["aRatioOfFixedToVarCost"]), times = N_PLAYERS)
                  ,aInitialPrice = rep(unname(parametros["aInitialPrice"]), times = N_PLAYERS)
                  ,aNormalProfitMargin = rep(unname(parametros["aNormalProfitMargin"]), times = N_PLAYERS)
                  ,aNormalCapacityUtilization = rep(unname(parametros["aNormalCapacityUtilization"]), times = N_PLAYERS)
                  #Target Capacity Sector
                  ,aMinimumEfficientScale = rep(unname(parametros["aMinimumEfficientScale"]), times = N_PLAYERS) # Original 100000
                  
                  # Esta variavel é desdobrada por player.
                  ,aDesiredMarketShare = c(unname(parametros["aDesiredMarketShare1"]), rep(unname(parametros["aDesiredMarketShare2"]), times = N_PLAYERS - 1) )      #rep(0.5, times = N_PLAYERS)
                  
                  # Esta variavel deve ser arredondada, sempre.
                  ,aSwitchForCapacityStrategy = round(c(unname(parametros["aSwitchForCapacityStrategy1"]), rep(unname(parametros["aSwitchForCapacityStrategy2"]),times = N_PLAYERS - 1)), 0)
                  
                  ,aWeightOnSupplyLine= rep(unname(parametros["aWeightOnSupplyLine"]), times = N_PLAYERS)
                  ,aTimeToPerceiveCompTargetCapacity = rep(unname(parametros["aTimeToPerceiveCompTargetCapacity"]), times = N_PLAYERS)
                  # Price Sector
                  ,aPriceAdjustmentTime = unname(parametros["aPriceAdjustmentTime"])
                  ,aSensOfPriceToCosts = rep(unname(parametros["aSensOfPriceToCosts"]), times = N_PLAYERS)
                  ,aSensOfPriceToDSBalance = rep(unname(parametros["aSensOfPriceToDSBalance"]), times = N_PLAYERS)
                  ,aSensOfPriceToShare = rep(unname(parametros["aSensOfPriceToShare"]), times = N_PLAYERS)
                  # Capacity Sector
                  ,aSwitchForPerfectCapacity = unname(parametros["aSwitchForPerfectCapacity"])
                  # A Initial Price
                  ,aInitialPrice = rep(unname(parametros["aInitialPrice"]), times = N_PLAYERS)
  )
  
  
  ##### VARIÁVEIS DE ENTRADA - ESTOQUES INICIAIS, SEM AJUSTES #####
  
  # Informando Estoques Iniciais, sem ajustes, apenas para calcular o primeiro tempo.
  stocks  <- c(
    sNPVProfit = rep(0, times = N_PLAYERS)
    ,sValueOfBacklog = rep(12738001, times = N_PLAYERS)
    ,sBacklog = rep(12738, times = N_PLAYERS) 
    ,sInstalledBase = rep(30000, times = N_PLAYERS) # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    ,sPrice = rep(1000, times = N_PLAYERS)
    ,sCumulativeAdopters = 60000 # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    ,sReportedIndustryVolume = rep(101904, times = N_PLAYERS)
    ,sCumulativeProduction = rep(1e+007, times = N_PLAYERS) # Este estoque possui formula
    ,sPerceivedCompTargetCapacity = rep(63690, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity1 = rep(63690, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity2 = rep(63690, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity3 = rep(63690, times = N_PLAYERS) # Este estoque possui formula
  ) 
  
  # Calculando estoques para o t0.
  estoques_calculados = modelo(time = 0, stocks = stocks, auxs = auxs, modo = "inicial")
  
  # Substituindo estoques no t0
  stocks  <- c(
    sNPVProfit = rep(0, times = N_PLAYERS)
    ,sValueOfBacklog = unname(estoques_calculados$ValueOfBacklogIni)
    ,sBacklog = unname(estoques_calculados$BacklogIni)
    ,sInstalledBase = rep(unname(estoques_calculados$InstalledBaseIni), times = N_PLAYERS)
    ,sPrice = unname(auxs$aInitialPrice)
    ,sCumulativeAdopters = unname(estoques_calculados$CumulativeAdoptersIni)
    ,sReportedIndustryVolume = rep(unname(estoques_calculados$ReportedIndustryVolumeIni), times = N_PLAYERS)
    ,sCumulativeProduction = unname(estoques_calculados$CumulativeProductionIni)
    ,sPerceivedCompTargetCapacity = unname(estoques_calculados$PerceivedCompTargetCapacityIni)
    ,sSmoothCapacity1 = unname(estoques_calculados$CapacityIni)
    ,sSmoothCapacity2 = unname(estoques_calculados$CapacityIni)
    ,sSmoothCapacity3 = unname(estoques_calculados$CapacityIni)
  ) 
  
  resultado_completo = data.frame(ode(y=stocks, simtime, func = modelo, 
                                      parms=auxs, method="euler"))
  # Posso filtrar os resultados ou não:
  # resultado_completo[variaveis_calibracao]
  resultado_completo
}

getCost<-function(parametros, modelo, dados_calibracao){
  
  output_modelo <- solve_modelo_dissertacao(parametros, modelo)
  #http://www.inside-r.org/packages/cran/FME/docs/modCost
  
  cost <- modCost(obs=dados_calibracao, model=output_modelo)
  
  return(cost)
  
}


##### GRÁFICOS ####
gerar_grafico_superficie = function(dados_ultimo_ano,variaveis, estrategia) {
  dadosplot = subset.data.frame(dados_ultimo_ano, (Lever == estrategia))
  
  dadosplot = dadosplot[variaveis]
  
  dadosplot = as.matrix(dadosplot)
  
  names = colnames(dadosplot)
  
  s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])
  
  names(s) = names
  
  # Plotando a População Final
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Taxa Nascimento",
    titlefont = f
  )
  y <- list(
    title = "TaxaMorte",
    titlefont = f
  )
  z <- list(
    title = "Populacao",
    titlefont = f
  )
  
  plot_ly(x = s[[1]], y = s[[2]], z = s[[3]]) %>% add_surface() %>% layout(xaxis = x, yaxis = y)
  
}

plot_clientes_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Clientes") + 
    xlab("Tempo") +
    labs(color = "Estratégia")
}

plot_cash_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Valor Presente") + 
    xlab("Tempo") +
    labs(color = "Estratégia")
}

plot_linha_uma_variavel_ensemble = function(dados, variavel, nome_amigavel_variavel, estrategia) {
  
  gr2_dados = subset(dados, (Lever %in% estrategia))
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(gr2_dados, aes(x= time, y= Variavel, color=factor(Lever) , group= interaction(Lever, Scenario) 
                                          )),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Tempo (anos)") + 
    theme(legend.position="bottom")  +
    labs(color = "Estratégia")
}


plot_linha_uma_variavel = function(dados, variavel, nome_amigavel_variavel) {
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= time, y= Variavel)),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Tempo (anos)") + 
    theme(legend.position="bottom")
}


plot_linha_duas_variaveis = function(dados, variavel1, nome_amigavel_variavel1, variavel2, nome_amigavel_variavel2) {
  
  p <- ggplot2::ggplot(dados, aes(x = time))
  
  call_variavel1 = substitute(
    expr = p + geom_line(aes(y = Variavel, colour = NomeVariavel)),
    env = list(Variavel = as.name(variavel1), NomeVariavel = nome_amigavel_variavel1)
  ) 
  
  razaovariavel = (max(dados[,variavel1]) - min(dados[,variavel1])) / (max(dados[,variavel2]) - min(dados[,variavel2]))
  
  p <- eval(call_variavel1)
  
  call_variavel2 = substitute(
    expr = p + geom_line(aes(y = Variavel * Razao, colour = NomeVariavel)),
    env = list(Variavel = as.name(variavel2), NomeVariavel = nome_amigavel_variavel2, Razao = razaovariavel)
  ) 
  
  p <- eval(call_variavel2)
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./razaovariavel, name = nome_amigavel_variavel2))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = nome_amigavel_variavel1,
                x = "Tempo (anos)",
                colour = "Variáveis")
  
  p <- p + theme(legend.position="bottom")

  p
  
}



plot_taxa_adocao_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adoption_Rate, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Taxa de Adoção") + 
    xlab("Tempo (anos)") +
    labs(color = "Estratégia")
}


#' grafico_whisker_por_lever
#'
#' Este grafico 
#' 
#' @param dados_regret dados resultantes da análise de regret.
#' @param variavel nome da variável simulada a realizar o gráfico.
#'
#' @return grafico whisker do ggplot2
#' @export
#'
grafico_whisker_por_lever = function(dados_regret, variavel) {
  dados_por_estrategia = dplyr::group_by(dados_regret, Lever)
  
  dados_por_estrategia$Lever = as.factor(dados_por_estrategia$Lever)
  
  # Gerando Grafico da Variável de Perda de Oportunidade
  call_grafico = substitute(
    expr = ggplot(dados_por_estrategia, aes(y = Variavel,x = Lever, group = Lever)),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  p + geom_boxplot()
}

#' plot_fronteira_tradeoff_estrategia
#' 
#' Esta funcao ainda nao é completamente generalizada. estao dentro desta funcao a definicao do cenário a ser analisado.
#'
#' @param results list com os dados resultantes da funcao de simulacao e analise
#' @param opcoes list de opcoes do modelo (as opcoes sao padronizadas.)
#'
#' @return grafico plotly com a fronteira de tradeoffs conforme um determinado cenário.
#' @export
#'
plot_fronteira_tradeoff_estrategia = function(results, opcoes) {
  
  dados_cenario = results$DadosUltimoPeriodo %>% filter(AdvertisingCost  < 5.727e+04 & AverageTicket  >  1.789e+00 & AdoptionFraction  <  2.895e-02)
  
  analise_regret_cenario = calcular_e_resumir_regret(dados = dados_cenario, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  variavel_comparacao = paste(opcoes$VarResposta,opcoes$VarCriterio, sep = "")
  
  variaveis_grafico_regret = c(opcoes$VarEstrategias, variavel_comparacao)
  
  regret_todos_os_futuros = results$AnaliseRegret$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_todos_os_futuros = as.data.frame(regret_todos_os_futuros)
  
  names(regret_todos_os_futuros) = c(opcoes$VarEstrategias, "PerdaOportunidadeTodosOsCenarios")
  
  # names(regret_todos_os_futuros[variaveis_grafico_regret]) = c(opcoes$VarEstrategias, paste(variavel_comparacao, "TodosOsCenarios", sep = ""))
  
  regret_cenario = analise_regret_cenario$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_cenario = as.data.frame(regret_cenario)
  
  names(regret_cenario) = c(opcoes$VarEstrategias, "PerdaOportunidadeNoCenario")
  
  dados_join = dplyr::left_join(regret_todos_os_futuros, regret_cenario)
  
  plot_ly(data = dados_join, x = ~PerdaOportunidadeTodosOsCenarios, y = ~PerdaOportunidadeNoCenario, color = ~Lever, text = ~Lever)
  
}


#' plot_estrategias_versus_incertezas
#'
#' @param ensemble_analisado data.frame resultante da funcao analisar_ensemble_com_melhor_estrategia
#' @param incertezas vetor com nomes das variáveis de incerteza a incluir no gráfico, devem corresponder à variáveis no ensemble.
#' @param binario default \code{TRUE}, pode apresentar a divisão entre as outras estratégias ou não.
#'
#' @return grafico que mostra a que condições a estratégia candidata é mais sucetível a falhar.
#' @export
plot_estrategias_versus_incertezas = function(ensemble_analisado, incertezas, binario = TRUE) {
  ensemble_analisado$EstrategiaCandidata = as.factor(ensemble_analisado$EstrategiaCandidata)
  
  ensemble_analisado$Lever = as.factor(ensemble_analisado$Lever)
  
  p = if(binario) {
    GGally::ggpairs(ensemble_analisado, columns = incertezas, aes(colour = EstrategiaCandidata, alpha = 0.7))
  } else {
    GGally::ggpairs(ensemble_analisado, columns = incertezas, aes(colour = Lever, alpha = 0.7))
  }
  
  p
}


sdrdm.pairs_plot= function(data, lever, variables) {
  dados_grafico = subset(data, Lever == lever)
  dados_grafico = dados_grafico[variables]
  
  pairs(dados_grafico)
}
