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