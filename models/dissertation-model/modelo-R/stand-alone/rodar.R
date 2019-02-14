# Local da Planilha com Dados de Entrada:
planilha_simulacao_opcao1_futuro = "params_calibracao_opcao1.xlsx"


# Carregando Funções Úteis
list_tabelas_output = list()
START<-2007; FINISH <-2017; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
N_PLAYERS <<- 4;
source('funcoes.R', encoding = 'UTF-8')
# Parâmetros para a Geração dos Gráficos
plots_width = 9
plots_heigh = 3.5

USAR_DADOS_SALVOS = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE


# Opções, mudando a Variável de Critério:
opcoes_iniciais = list(
  VarResposta = "sNPVProfit1",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 30,
  VarTempo = "time",
  VarCriterio = "RegretPercentil75",
  SentidoCriterio = "min",
  Paralelo = TRUE,
  ModoParalelo = "PSOCK", # PSOCK - Windows e Linux. FORK - Apenas UNIX
  SimularApenasCasoBase = TRUE,
  FullFactorialDesign = TRUE,
  FiltrarCasosPlausiveis = TRUE
)

opcoes = opcoes_iniciais


n_estrategias = nrow(carregar_inputs(arquivo_de_inputs = planilha_simulacao_opcao1_futuro, opcoes = opcoes)$Levers)

# Tamanho do Ensemble Adimitido (para simular todas as estratégias)


#### 4.2 Simulação dos Casos Contra Estratégias ####
# Esta opção é inspirada na abordagem utilizada por Lempert.
opcoes$FiltrarCasosPlausiveis = FALSE
opcoes$SimularApenasCasoBase = FALSE
opcoes$N = n_ensemble_total
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_opcao1_futuro
START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')

vetor_num_de_cenarios = c(5)

for(i in vetor_num_de_cenarios) {
  print(i)
  
  # Definir o Numero de Rodadas a Realizar
  opcoes$N = i
  
  # In?cio da Simulacao
  print(paste0("iniciando a simulacao com o seguinte n?mero de cen?rios : ",i))
  Sys.time()
  
  # Simular
  results = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                             sdmodel = sdmodel, 
                                             opcoes = opcoes)
  names(results$DadosSimulados)
  
  # Observando apenas duas medi??es por ano para economizar espa?o:
  ano_inicial = min(results$DadosSimulados$time)
  ano_final = max(results$DadosSimulados$time)
  vetor_tempo = seq.default(from = ano_inicial, to = ano_final, length.out = 21)
  
  results$DadosSimulados = subset(results$DadosSimulados, time %in% vetor_tempo)
  
  saveRDS(results, paste0(i,"results.RDS"))

  # Salvar os Resultados
  # save(results, file = paste0(i, "results.rda"))
  
  Sys.sleep(10)
  
}

# Simular
# results = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
#                                                 sdmodel = sdmodel, 
#                                                 opcoes = opcoes)




