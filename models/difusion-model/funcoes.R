library(lhs)
library(deSolve)

##### CARREGAR INPUTS #####

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
  variaveis = c(c("Scenario"),variaveis)
  
  colnames(ensemble) = variaveis
  
  ensemble[,"Scenario"] = 1:nrow(ensemble)
  
  ensemble
}

##### AMPLIAR ENSEMBLE COM ESTRATÉGIAS #####

ampliar_ensemble_com_levers = function(ensemble, levers) {
  
  variaveis_adicionais = names(dplyr::select(levers, -LeverCode))
  
  linhas_ensemble_incial = nrow(ensemble)
  novo_ensemble = matrix(0, nrow = nrow(ensemble)*length(levers$Lever), ncol = ncol(ensemble) + length(variaveis_adicionais))
  
  names_old_ensemble = colnames(ensemble)
  names_novo_ensemble = c(names_old_ensemble, variaveis_adicionais)
  
  colnames(novo_ensemble) = names_novo_ensemble
  
  j = 1
  for (l in seq_along(inputs$Levers$Lever)) {
    lini = j
    lfim = j + linhas_ensemble_incial-1
    matriz_var_adicionais = as.matrix(inputs$Levers[l,variaveis_adicionais])
    novo_ensemble[lini:lfim,names_old_ensemble] = ensemble
    novo_ensemble[lini:lfim,variaveis_adicionais] = matrix(matriz_var_adicionais, nrow = linhas_ensemble_incial, ncol = ncol(matriz_var_adicionais), byrow = TRUE)
    j = j + linhas_ensemble_incial
  }
  
  novo_ensemble
  
}


##### SIMULAR #####

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
    dados_simulacao[l_inicial:l_final,ncolunas] = ensemble[i,"Scenario"]
    
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

##### FUNÇÕES AUXILIARES #####

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}



