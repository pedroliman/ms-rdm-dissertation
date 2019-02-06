library(devtools)
# install the doAzureParallel and rAzureBatch package
devtools::install_github("Azure/rAzureBatch")
devtools::install_github("Azure/doAzureParallel")

getwd()

library(doAzureParallel)

#### 4.2 Simulação dos Casos Contra Estratégias ####
# Esta opção é inspirada na abordagem utilizada por Lempert.
opcoes$FiltrarCasosPlausiveis = FALSE
opcoes$SimularApenasCasoBase = FALSE
opcoes$N = 10 # n_ensemble_total
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_opcao1_futuro
START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')

opcoes$ModoParalelo = "Azure"

# Simular
results_teste = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                                 sdmodel = sdmodel, 
                                                 opcoes = opcoes)






#### Testes anteriores ####
generateCredentialsConfig("credentials.json")

# 3. Set your credentials - you need to give the R session your credentials to interact with Azure
setCredentials("credentials.json")

# 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
cluster <- doAzureParallel::makeCluster(cluster = "cluster.json")

# 5. Register the pool as your parallel backend
registerDoAzureParallel(cluster)

# 6. Check that your parallel backend has been registered
getDoParWorkers()

# 5. Register the cluster as your parallel backend
registerDoAzureParallel(cluster)


# Demo:
mean_change = 1.001
volatility = 0.01
opening_price = 100

simulateMovement <- function() {
  days <- 1825 # ~ 5 years
  movement <- rnorm(days, mean=mean_change, sd=volatility)
  path <- cumprod(c(opening_price, movement))
  return(path)
} 

simulations <- replicate(30, simulateMovement())
matplot(simulations, type='l') # plots all 30 simulations on a graph

getClosingPrice <- function() {
  days <- 1825 # ~ 5 years
  movement <- rnorm(days, mean=mean_change, sd=volatility)
  path <- cumprod(c(opening_price, movement))
  closingPrice <- path[days]
  return(closingPrice)
}



funcao_i = function(i, a) {
  i * 100 * a
}

# Ele demora bastante para enviar cada uma das "tarefas", o que parece que é a unidade não-divisível do trabalho. Talvez cada uma das estratégias seja uma tarefa.
closingPrices <- foreach(i = 1:50, .combine='c') %dopar% {
  replicate(100, getClosingPrice())
}
a = 2

teste_i <- foreach(i = 1:5, .combine='c') %dopar% {
  a = 3
  funcao_i(i, a)
}

?foreach

# Depois de rodar, parar o cluster:
# shut down your pool
stopCluster(cluster)
