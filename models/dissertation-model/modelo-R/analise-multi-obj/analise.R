
#### Índices de Robustez #####


source("./../funcoes.R")

# Carregando Dados da simulação
# Salvar Resultados com apenas 10 anos simulados.

results_path = "C:/Temporario/rdm-results-backup/"

#save(results, file = paste0(results_path,"results_final.rda"))

#results = readRDS()

load(paste0(results_path,"results_final.rda"))


# Tornando os resultados mais leves:

# Observando apenas duas medi??es por ano para economizar espa?o:
ano_inicial = min(results$DadosSimulados$time)
ano_final = max(results$DadosSimulados$time)
vetor_tempo = seq.default(from = ano_inicial, to = ano_final, length.out = 21)

results$DadosSimulados = subset(results$DadosSimulados, time %in% vetor_tempo)






# Formas Simples de Calcular Índices de Robustez:
robustness = function(x) {
  mean(x)/sd(x)
}

percentile_75 = function(x) {
  quantile(x, probs = c(0.75))
}

## Calculando Resultados Adicionais no Último Período

# Calculando algumas variáveis com base nos dados Simulados:

results$DadosUltimoPeriodo$sPriceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("sPrice1", "sPrice2", "sPrice3", "sPrice4")])

results$DadosUltimoPeriodo$aPerformanceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("aPerformance1", "aPerformance2", "aPerformance3", "aPerformance4")])

results$DadosUltimoPeriodo$sTotalInstalledBase = rowSums(results$DadosUltimoPeriodo[,c("sInstalledBase1", "sInstalledBase2", "sInstalledBase3", "sInstalledBase4")])

## Analisando o Regret de Diversas Variáveis:

analise_regret_profit = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = results$Opcoes$VarResposta, var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_installed_base = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sInstalledBase1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_share = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aOrderShare1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_performance = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPerformance1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_patentes = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPatentesEmpresaTemAcesso1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_adopters  = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sCumulativeAdopters", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_average_price  = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sPriceAvg", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias, sentido = "min")$ResumoEstrategias

analise_regret_average_performance  = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPerformanceAvg", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias

analise_regret_total_installed_base  = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sTotalInstalledBase", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)$ResumoEstrategias


# Unindo Datasets:

dataset_todas_as_metricas = left_join(analise_regret_profit, analise_regret_installed_base, by = "Lever") %>%
  left_join(., analise_regret_share, by = "Lever") %>% 
  left_join(., analise_regret_performance, by = "Lever") %>%
  left_join(., analise_regret_patentes, by = "Lever") %>%
  left_join(., analise_regret_adopters, by = "Lever") %>% 
  left_join(., analise_regret_average_price, by = "Lever") %>%
  left_join(., analise_regret_average_performance, by = "Lever") %>%
  left_join(., analise_regret_total_installed_base, by = "Lever")

# Filtrando Só o que eu quero usar e Unindo com a Descrição dos Levers:
dataset_todas_as_metricas  = dataset_todas_as_metricas %>%
  dplyr::select(., matches("RegretPercentil75|Lever")) %>%
  left_join(., results$Inputs$Levers, by = "Lever") 

# Escrever para um arquivo:

write.csv(dataset_todas_as_metricas, file = "output_regret_metricas.csv", row.names = F)

View(dataset_todas_as_metricas)

dados_simulados = results$DadosSimulados %>% 
  group_by(time, Lever) %>%
  summarise_all(funs(mean, sd, robustness, percentile_75))


dados_simulados = dplyr::inner_join(dados_simulados, results$Inputs$Levers)

dados_ultimo_ano = dados_simulados[dados_simulados$time == 2028,]

View(dados_ultimo_ano)

# Funcionalidade
write.csv(x = dados_simulados, file = "output_analise_multi_objetivo.csv", row.names = F)

write.csv(x = dados_ultimo_ano, file = "output_analise_multi_objetivo.csv", row.names = F)


plot(x = dados_simulados$aSwitchForCapacityStrategy1)
