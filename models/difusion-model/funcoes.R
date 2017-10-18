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

##### CONSTANTES #####
VAR_SCENARIO = "Scenario"


##### SIMULAR RDM E ESCOLHER ESTRATEGIA #####

#' simularRDM_e_escolher_estrategia
#'
#' @param inputs caminho para o arquivo de inputs
#' @param sdmodel list com modelo e suas opções
#' @param opcoes list com opções para a simulação e analise de Regret.
#'
#' @return list com resultados da simulacao e uma estratégia candidata.
simularRDM_e_escolher_estrategia = function(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes) {
  ## Simular
  dados_simulacao = simular_RDM(arquivo_de_inputs=inputs ,sdmodel = sdmodel, n = opcoes$N)
  
  # Selecionando dados do último ano:
  dados = selecionar_ultimo_periodo(dados_simulacao = dados_simulacao, var_tempo = opcoes$VarTempo)
  
  # Analisar Regret
  analise_regret = calcular_e_resumir_regret(dados = dados, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  # Escolher a Estratégia Candidata
  estrategia_candidata = escolher_estrategia_candidata(dados = analise_regret$Dados, resumo_estrategias = analise_regret$ResumoEstrategias, var_resposta = opcoes$VarResposta, var_criterio = opcoes$VarCriterio, sentido = opcoes$SentidoCriterio)
  
  message(paste("A Estrategia candidata é a ", estrategia_candidata$Lever))
  
  output = list(
    DadosSimulados = dados_simulacao,
    DadosUltimoPeriodo = dados,
    AnaliseRegret = analise_regret,
    EstrategiaCandidata =  as.numeric(estrategia_candidata[opcoes$VarEstrategias]) 
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
simular = function(stocks, simtime, modelo, ensemble, nomes_variaveis_final) {
  message("01. funcoes.R/simular: Iniciando Simulação.")
  # Rodando a Simulação (uma vez), com a primeira linha do ensemble - Ajuda a saber se funciona.
  # Esta função apenas funciona com o estoque inicial fixo, será necessário implementar de outra forma depois.
  o<-data.frame(ode(y=stocks, times=simtime, func = modelo, 
                    parms=ensemble[1,], method="euler"))
  pontos = nrow(ensemble)
  
  nlinhas = nrow(o)
  
  ncolunas = ncol(o)+1
  
  # Montando uma matriz com todos os dados para a simulação
  dados_simulacao = matrix(nrow = pontos*nlinhas, ncol = ncolunas)
  
  # J é o índice dos dados simulados
  j = 1
  # Rodando a Simulacao Em todo o Ensemble
  for (i in 1:nrow(ensemble)) {
    resultados_simulacao = ode(y=stocks, times=simtime, func = modelo, 
                               parms=ensemble[i,], method="euler")
    linhas = nrow(resultados_simulacao)
    
    
    # Avançando a linha inicial e Final da Simulação
    l_inicial = j
    l_final = j + linhas-1
    
    # Adicionando o resultado ao ensemble
    dados_simulacao[l_inicial:l_final,1:ncolunas-1] = resultados_simulacao
    
    # Adicionando o Número do Cenário
    dados_simulacao[l_inicial:l_final,ncolunas] = ensemble[i,VAR_SCENARIO]
    
    # Exibindo uma Mensagem de Status
    if (i %% 100 == 0) {
      message(paste(i, "simulações finalizadas."))
    }
    
    # Avançando o índice dos dados simulados
    j = j + linhas
  }
  
  colnames(dados_simulacao) = nomes_variaveis_final
  
  dados_simulacao = as.data.frame(dados_simulacao)
  names(dados_simulacao) = nomes_variaveis_final
  
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
  
  message(paste("Esta rotina realizará", nestrategias * nfuturos * ntempo, "Simulacoes.\n (", nestrategias, "estratégias x", nfuturos, "futuros, em", ntempo , "periodos de tempo."))
  
  dados_simulacao = simular(stocks = sdmodel$Stocks, simtime = sdmodel$SimTime, modelo = sdmodel$Modelo, ensemble = novo_ensemble, nomes_variaveis_final = sdmodel$Variaveis)
  
  t_fim = Sys.time()
  
  message("Finalizando Simulacao. Tempo de Simulacao: ", t_fim - t_inicio)
  
  dados_simulacao
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


##### CALCULO DO REGRET (PERDA DE OPORTUNIDADE) #####
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



##### ESCOLHER ESTRATÉGIA MINIMIZANDO #####

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


plot_taxa_adocao_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adoption_Rate, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Taxa de Adoção") + 
    xlab("Tempo") +
    labs(color = "Estratégia")
}


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
