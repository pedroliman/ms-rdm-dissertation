library(lhs)
library(deSolve)

##### CARREGAR INPUTS #####

carregar_inputs = function (arquivo_de_inputs="params.xlsx", abas_a_ler = c("params"), nomes_inputs = c("Parametros")) {
  
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


##### OBTER ENSEMBLE #####

obter_lhs_ensemble = function (params, n=100) {
  message("01. funcoes.R/obter_lhs_ensemble: Iniciando Obtenção do Ensemble.")
  #Obtendo DataFrame de Parâmetros
  
  ##### Sampling #####
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
  ensemble = matrix(nrow = pontos, ncol = nvar)
  
  # Montando o Ensemble
  for (var in variaveis) {
    i = which(x = variaveis == var)
    ensemble[,i] = qunif(p = randomLHS[,i], min = min[i], max = max[i])
  }
  colnames(ensemble) = variaveis
  
  ensemble
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
  print("Rodando Interações.")
  # Rodando a Simulacao Em todo o Ensemble
  for (i in 1:nrow(ensemble)) {
    resultados_simulacao = ode(y=stocks, times=simtime, func = modelo, 
                               parms=ensemble[i,], method="euler")
    linhas = nrow(resultados_simulacao)
    l_inicial = j
    l_final = j + linhas-1
    dados_simulacao[l_inicial:l_final,1:ncolunas-1] = resultados_simulacao
    dados_simulacao[l_inicial:l_final,ncolunas] = i
    j = j + linhas
  }
  
  colnames(dados_simulacao) = nomes_variaveis_final
  
  dados_simulacao = as.data.frame(dados_simulacao)
  names(dados_simulacao) = nomes_variaveis_final
  
  message("01. funcoes.R/simular: Finalizando Simulacao.")
  
  dados_simulacao
}