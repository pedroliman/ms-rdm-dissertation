library(lhs)

# Funções Auxiliares

carregar_inputs = function (arquivo_de_inputs="params.xlsx", abas_a_ler = c("params"), nomes_inputs = c("Parametros")) {
  
  # Criando uma list para os inputs
  message(
    paste("01. dados.R/carregar_inputs: Iniciando Carregamento de Inputs (funcao carregar_inputs()",
          "arquivo_de_inputs = ", arquivo_de_inputs)
  )
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs
  
  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }
  
  message("01. dados.R/carregar_inputs: Finalizando Carregamento de Inputs.")
  return(inputs)
  
}


obter_lhs_ensemble = function (params, n=100) {
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