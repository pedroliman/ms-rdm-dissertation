####
# Autor: Pedro Nascimento de Lima, 2017
# Código fonte desenvolvido para a Dissertação de Mestrado.
# Arquivo: amalise.R
# Objetivo: Este arquivo contém a execução da análise.
# RDM realizadas durante a dissertação.
####

library(randomForest)
library(pdp)
library(gridExtra)

####Configurando Simulações ####

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

# Gerar casos para simulação com base em estimativa inicial de parâmetros.

# Planilhas de Configuração das Simulação:
planilha_simulacao_calibracao_historico = "./calibracao/params_calibracao_historico.xlsx"

planilha_simulacao_opcao1_futuro = "./calibracao/params_calibracao_opcao1.xlsx"

planilha_opcao2.0_passado_e_futuro = planilha_simulacao_calibracao_historico

planilha_opcao2.1_futuro = planilha_simulacao_calibracao_historico

percentil_utilizado_como_criterio = c(PercentilCriterio = 0.5)

# Número de casos TOTAL a rodar (considerando todas as estratégias e todos os cenários).

# Original:
# n_casos_total = 54 * 200
n_casos_total = 54 * 200

n_estrategias = nrow(carregar_inputs(arquivo_de_inputs = planilha_simulacao_opcao1_futuro, opcoes = opcoes)$Levers)

# Tamanho do Ensemble Adimitido (para simular todas as estratégias)
n_ensemble_total = round(n_casos_total / n_estrategias, 0) 

# Tamanho do ensemble para calibração.
n_ensemble_calibracao = round(n_ensemble_total / percentil_utilizado_como_criterio,0)


####Verificação - Comparação de Simulações com o Ithink  - Não rodar ####
# # Rodando um Cenário Base
# ## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
# opcoes$FullFactorialDesign = FALSE
# opcoes$Paralelo = FALSE
# START<-0; FINISH<-10; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
# VERIFICAR_STOCKS = TRUE; VERIFICAR_CHECKS = TRUE; CHECK_PRECISION = 0.1; BROWSE_ON_DIFF = TRUE
# VERIFICAR_GLOBAL = TRUE;
# ## Carregando Modelo
# source('funcoes.R', encoding = 'UTF-8')
# 
# # Carregando Variáveis de Output do Ithink para Comparação
# arquivo_excel_stocks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_stocks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
# arquivo_excel_checks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_checks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
# arquivo_excel_global = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_tudo.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
# 
# 
# dados_ithink_stocks  = arquivo_excel_stocks$ResultadosIthink %>% dplyr::select(-Months)
# dados_ithink_checks  = arquivo_excel_checks$ResultadosIthink %>% dplyr::select(-Months)
# 
# dados_ithink_global = arquivo_excel_global$ResultadosIthink %>% dplyr::select(-Months)
# 
# 
# variaveis_ithink_stocks = names(dados_ithink_stocks)
# variaveis_ithink_checks = names(dados_ithink_checks)
# 
# variaveis_globais_a_verificar = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/variaveis_globais_a_verificar.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
# variaveis_globais_a_verificar = as.vector(variaveis_globais_a_verificar$ResultadosIthink$variaveis)
# 
# parametros_completos = readxl::read_xlsx(planilha_simulacao_calibracao_historico, sheet = "params")
# 
# parametros_cenariobase = t(parametros_completos[,"CenarioBase"])[1,]
# 
# names(parametros_cenariobase) = as.matrix(parametros_completos[,1])
# 
# 
# resultados_caso_base = solve_modelo_dissertacao(parametros = parametros_cenariobase, modelo = sdmodel$Modelo, simtime = sdmodel$SimTime)


#### 4.1 Teste com Dados Históricos de Demanda ####
# Simulação 0: Simulando Histórico e Observando Fit do Modelo:
###
opcoes$SimularApenasCasoBase = TRUE
opcoes$N = 200
# Esta opção faz com que os estoques sejam inicializados com o valor inicial dos estoques no cenário base.
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
# Esta opção é utilizada para modificar o comportamento de variáveis selecionadas (ex.: Estratégia do player) durante o período histórico.
# Se ativado, o período de tempo anterior ao ANO_INICIO_AVALIACAO assume variáveis com um valor "default", e o NPV dos players não é
# modificado enquanto até o ANO_INICIO_AVALIACAO. Se não ativado, a simulação ocorre normalmente.
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_calibracao_historico
opcoes$FiltrarCasosPlausiveis = FALSE
# Rodar Simulação:
START<-2004; FINISH <-2014; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')
resultados_casos_plausiveis = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                                               sdmodel = sdmodel,
                                                               opcoes = opcoes)




list_tabelas_output[["ParametrosCalibracao"]] <- resultados_casos_plausiveis$Inputs$Parametros

# Salvar resultados com casos plausíveis:
#save(resultados_casos_plausiveis, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/resultados_casos_plausiveis.rda")
save(resultados_casos_plausiveis, file = "resultados_casos_plausiveis.rda")

load("resultados_casos_plausiveis.rda")




# load(file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/resultados_casos_plausiveis.rda")


variavel_calibracao = "fIndustryOrderRate"
variavel_calibracao = "aIndustryShipments"
nome_amigavel_variavel_calibracao = "Demanda Global"

# Parâmetros Utilizados
resultados_casos_plausiveis$Inputs$Parametros

# Mostrar Ensemble.

head(resultados_casos_plausiveis$Ensemble, 10)
# Gerar 10.000 replicações e salvar.

# Mostrar Dados Simulados
head(resultados_casos_plausiveis$DadosSimulados, 20)

# Mostrar Variável de Demanda em Todos os Cenários (Sem Filtro)

cenarios_a_exibir_grafico = sample(1:opcoes$N,size = min(50,opcoes$N))


plot_demanda_pre_calibracao = plot_linha_uma_variavel_ensemble(dados = subset(resultados_casos_plausiveis$DadosSimulados, Scenario %in% cenarios_a_exibir_grafico), 
                                 variavel = variavel_calibracao, 
                                 nome_amigavel_variavel = nome_amigavel_variavel_calibracao) #+ geom_vline(xintercept = 2017)

plot_demanda_pre_calibracao = plot_demanda_pre_calibracao # + annotate("text", x = 2017.2, y = max(resultados_casos_plausiveis$DadosSimulados$fIndustryOrderRate), label=c("Hoje"),hjust=0)

plot_demanda_pre_calibracao

# Comparar Simulações com Dados históricos de Demanda
ensemble_com_erro = adicionar_erro_ao_ensemble(results = resultados_casos_plausiveis, variavel_calibracao = variavel_calibracao, planilha_calibracao = "./calibracao/dados_calibracao.xlsx", opcoes = opcoes)
ensemble_com_erro = as.data.frame(ensemble_com_erro)


# Exibir Comparação com Dados Históricos.

variaveis_analise_fit = c("RSquared", "r", "RootMeanSquareError", "SumOfSquareResiduals", "MeanSquareError", "MeanAbsoluteError", "MeanAbsolutePercentError", "UM_ThielBiasDiffMeans", "US_ThielUnequalVariation", "UC_ThielUnequalCovariation")
variaveis_exibir_ensemble = c("Scenario", variaveis_analise_fit)
cenarios_a_exibir_tabela = sample(1:opcoes$N,size = 5)

# Demonstração do erro calculado
tabela_de_erro_calculado = t(ensemble_com_erro[1:5,variaveis_exibir_ensemble])

tabela_de_erro_calculado = as.data.frame(tabela_de_erro_calculado)

tabela_de_erro_calculado[,"VariavelCalculada"] = rownames(tabela_de_erro_calculado)

list_tabelas_output[["CalibracaoErroCalculado"]] <- tabela_de_erro_calculado

tabela_de_erro_calculado

# Exibindo Ordem de Grandeza dos Erros Médios Absolutos

histograma_erro_percentual = ggplot(ensemble_com_erro, aes(x=MeanAbsolutePercentError)) + 
                                geom_histogram(aes(y=..density..), colour="black", fill="white")+
                                geom_density(alpha=.2, fill="#FF6666") +
                                xlab("Erro Médio Percentual") + 
                                ylab("Densidade")


histograma_erro_percentual


histograma_erro_medio_quadrado = ggplot(ensemble_com_erro, aes(x=MeanSquareError)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Erro Médio Quadrado") + 
  ylab("Densidade")

histograma_erro_medio_quadrado


# Exibir caso com melhor Fit
cenario_menor_erro = ensemble_com_erro[which(ensemble_com_erro[,"MeanSquareError"]==min(ensemble_com_erro[,"MeanSquareError"])),opcoes$VarCenarios]

# Exibir parâmetros do cenario com menor erro:

params_cenario_menor_erro = ensemble_com_erro[which(ensemble_com_erro[,opcoes$VarCenarios]==cenario_menor_erro),]

parametros_cenario_menor_erro = t(ensemble_com_erro[which(ensemble_com_erro[,opcoes$VarCenarios]==cenario_menor_erro),])

parametros_cenario_menor_erro

write.csv2(parametros_cenario_menor_erro, "output_calibracao.csv")

parametros_cenario_menor_erro = as.data.frame(parametros_cenario_menor_erro)

parametros_cenario_menor_erro[,"Parâmetro"] = rownames(parametros_cenario_menor_erro)

list_tabelas_output[["ParametrosCenarioMenorErro"]] <- parametros_cenario_menor_erro

# Condições Iniciais do cenário com Menor Erro:

resultados_casos_plausiveis$DadosUltimoPeriodo[which(resultados_casos_plausiveis$DadosUltimoPeriodo$Scenario==cenario_menor_erro),]

# Condições Finais do Cenário com Menor erro (pode ser usado como base):

VARIAVEIS_FINAIS_CASO_BASE = resultados_casos_plausiveis$DadosUltimoPeriodo[which(resultados_casos_plausiveis$DadosUltimoPeriodo$Scenario==cenario_menor_erro),]

# Plotar Gráfico do Cenário com Menor Erro:
# Selecionando Pontos de Dados para Exibir no Gráfico
time_points<-seq(from=1, to=length(SIM_TIME),by=1/STEP)
time_plot = seq(from=START, to=FINISH)
resultados_exibir = dplyr::filter(resultados_casos_plausiveis$DadosSimulados, Scenario == cenario_menor_erro)[time_points,]

# Exibir Demanda Global.
dados_calibracao <- as.data.frame(read_xlsx(path = "./calibracao/dados_calibracao.xlsx", sheet = "Plan1"))
# Exibir Share dos Players e outras variáveis.

plot_cenario_base_e_historico <-ggplot()+
  geom_point(data=dados_calibracao,size=1.5,aes(time,aIndustryShipments,colour="Data"))+
  geom_line(data=resultados_exibir,size=1,aes(x=time,y=aIndustryShipments,colour="Model"))+
  ylab("Professional 3D Printers Demand")+
  xlab("Years")+
  scale_y_continuous(labels = format_for_humans)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Data",
                               "Calibrated Model")) + 
  ggtitle("Model fit to Historical Data")
plot_cenario_base_e_historico 



# Definição de Casos Considerados Plausíveis para a Geração de Ensemble 2.0 e 2.1

percentil_ssr = quantile(ensemble_com_erro[,"SumOfSquareResiduals"], probs = c(percentil_utilizado_como_criterio))

cenarios_quartis = ensemble_com_erro[which(ensemble_com_erro[,"SumOfSquareResiduals"] <= percentil_ssr),opcoes$VarCenarios]

cenarios_considerados_plausiveis = cenarios_quartis

# Como critério irei utilizar apenas os casos que erram em média menos do que 30 %.
#erro_percentual_medio_maximo = 0.5

dados_calibracao$Scenario = 1000

# cenarios_considerados_plausiveis = ensemble_com_erro[which(ensemble_com_erro$MeanAbsolutePercentError < erro_percentual_medio_maximo),opcoes$VarCenarios]

plot_cenarios_plausiveis = plot_linha_uma_variavel_ensemble(dados = resultados_casos_plausiveis$DadosSimulados[which(resultados_casos_plausiveis$DadosSimulados$Scenario %in% cenarios_considerados_plausiveis),]
                                 ,variavel = variavel_calibracao, nome_amigavel_variavel = nome_amigavel_variavel_calibracao) + geom_point(data=dados_calibracao, size = 1.5, aes(time, aIndustryShipments))

plot_cenarios_plausiveis



# Gerando Gráficos da Calibração
plots_calibracao = list(
  calibracao_plot_cenarios_plausiveis = plot_cenarios_plausiveis,
  calibracao_plot_cenario_base_e_historico = plot_cenario_base_e_historico,
  calibracao_plot_demanda_pre_calibracao = plot_demanda_pre_calibracao,
  calibracao_histograma_erro_percentual = histograma_erro_percentual,
  calibracao_histograma_erro_medio_quadrado = histograma_erro_medio_quadrado
)

# Salvando Gráficos da Calibração.
mapply(ggsave, file=paste0("./images/", names(plots_calibracao), ".png"), plot=plots_calibracao, width = plots_width, height = plots_heigh)


tabelas_calibracao = list(
  calibracao_parametros_cenario_menor_erro = parametros_cenario_menor_erro,
  calibracao_tabela_de_erro_calculado = tabela_de_erro_calculado
)

# Salvando Tabelas da Calibração:
mapply(write.csv2, 
       x = tabelas_calibracao, 
       file = paste0("./tabelas/",names(tabelas_calibracao),".csv")
       )



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

# Simular
# results = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
#                                            sdmodel = sdmodel, 
#                                            opcoes = opcoes)

# Salvar Resultados com apenas 10 anos simulados.

results_path = "C:/Temporario/rdm-results-backup/"

#save(results, file = paste0(results_path,"results_final.rda"))

load(paste0(results_path,"results_final.rda"))

# Aplicando o Filtro de plausibilidade (Nenhum resultado caiu no filtro)

#### 4.3 Análise dos Resultados ####

# Escolher Estratégia Candidata de acordo com a definição de Critério:
results$EstrategiaCandidata = escolher_estrategia_candidata(dados = results$AnaliseRegret$Dados, resumo_estrategias = results$AnaliseRegret$ResumoEstrategias, var_resposta = opcoes$VarResposta, var_criterio = opcoes$VarCriterio, sentido = opcoes$SentidoCriterio)


# Gerar Gráficos para Analisar os Resultados:

# Gerando Resultados da Opção 1 - Estratégia Candidata 1, e Cenário 1
plots_results = salvar_plots_result(results = results, 
                                    cenario_plot_players = results$DadosUltimoPeriodo$Scenario[100],
                                    estrategia_candidata = results$EstrategiaCandidata$Lever[1],
                                    opcoes = opcoes)

plots_results$plots_linha_geral$plot_estrategia_candidata_demanda_global

plots_results$plots_whisker$plot_whisker_lever_regret

plots_results$plots_whisker$plot_whisker_lever_profit

plots_results$plots_whisker$plot_whisker_lever_share

grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aPerformance1", "Player 1 Product Performance")


# Observando Resultados

plots_results$plots_whisker$plot_whisker_lever_regret

plots_results$plots_whisker$plot_whisker_lever_profit

plots_results$plots_whisker$plot_whisker_lever_share


#### Gerando Ranking de Estratégias ####

ranking_estrategias = results$AnaliseRegret$ResumoEstrategias[,c("Lever", "sNPVProfit1RegretPercPercentil75", "sNPVProfit1RegretPercentil75")]

ranking_estrategias = dplyr::inner_join(ranking_estrategias, results$Inputs$Levers)

ranking_estrategias = dplyr::arrange(ranking_estrategias, sNPVProfit1RegretPercentil75)

ranking_estrategias_formatado = ranking_estrategias

ranking_estrategias_formatado$sNPVProfit1RegretPercentil75 = format_for_humans(ranking_estrategias_formatado$sNPVProfit1RegretPercentil75)

View(ranking_estrategias)

list_tabelas_output[["RankingEstrategias"]] <- ranking_estrategias

resultados_analise = list(
  plots_results = plots_results,
  ranking_estrategias = ranking_estrategias
)


#### 4.3 Análise de Vulnerabilidade ####

# Rodando análise de Vulnerabilidade com cada Futuro:

# Visualizando um histograma do Regret da Estratégia Candidata

regret_perc_estrategia_candidata = results$AnaliseRegret$Dados$sNPVProfit1RegretPerc[which(results$AnaliseRegret$Dados$Lever == results$EstrategiaCandidata$Lever)]


regret_estrategia_candidata = results$AnaliseRegret$Dados$sNPVProfit1Regret[which(results$AnaliseRegret$Dados$Lever == results$EstrategiaCandidata$Lever)]


# O critério para realizar a análise de vulnerabilidade foi o mesmo usado para definir a estratégia candidata:
threshold_analise_vulnerabilidade = as.numeric(results$AnaliseRegret$ResumoEstrategias[which(results$AnaliseRegret$ResumoEstrategias$Lever==results$EstrategiaCandidata$Lever),paste(opcoes$VarResposta, opcoes$VarCriterio, sep="")]) 

# threshold_analise_vulnerabilidade = 0.35

# threshold_analise_vulnerabilidade = 5*10^8


histograma_regret_estrategia_candidata = ggplot(results$AnaliseRegret$Dados[which(results$AnaliseRegret$Dados$Lever == results$EstrategiaCandidata$Lever),], aes(x=sNPVProfit1Regret)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Perda de Oportunidade") + 
  ylab("Densidade") + 
  geom_vline(xintercept = threshold_analise_vulnerabilidade) +
  scale_x_continuous(labels = format_for_humans) 

y_max = max(ggplot_build(histograma_regret_estrategia_candidata)$layout$panel_ranges[[1]]$y.rang)

histograma_regret_estrategia_candidata = histograma_regret_estrategia_candidata + annotate("text", x = threshold_analise_vulnerabilidade + 0.05, y = y_max, label=c("-> Casos de Interesse"),hjust=0)

histograma_regret_estrategia_candidata

# Gerando DataFrame propício para a análise de vulnerabilidade.
df_vulnerabilidade = obter_df_vulnerabilidade(results = results, 
                                              estrategia_candidata = results$EstrategiaCandidata$Lever, 
                                              variavel_resposta = "sNPVProfit1Regret" , 
                                              threshold = threshold_analise_vulnerabilidade, 
                                              planilha_inputs = planilha_inputs, 
                                              sentido_vulnerabilidade = ">=")

ensemble_analisado_melhor_estrategia = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble,
                                                             dados_regret = results$AnaliseRegret$Dados, 
                                                             var_cenarios = opcoes$VarCenarios, 
                                                             var_estrategias = opcoes$VarEstrategias, 
                                                             var_resposta = opcoes$VarResposta, 
                                                             estrategia_candidata = results$EstrategiaCandidata$Lever)


# melhor_estrategia_por_cenario = ensemble_analisado_melhor_estrategia %>% dplyr::select(Scenario, Lever)
# 
# names(melhor_estrategia_por_cenario) = c("Scenario", "MelhorEstrategia")
# 
# df_vulnerabilidade = dplyr::inner_join(melhor_estrategia_por_cenario, df_vulnerabilidade)

View(df_vulnerabilidade)


# Gerando Y e X para as Análises de Vulnerabilidade:
y = factor(df_vulnerabilidade$CasoInteresse)

y_continuo = df_vulnerabilidade$sNPVProfit1Regret
x = df_vulnerabilidade[,5:ncol(df_vulnerabilidade)]

list_tabelas_output[["DataFraneVulnerabilidade"]] <- df_vulnerabilidade


#### 4.3.1 - Ranking de Variáveis considerando apenas médias.####
ranking_variaveis_por_media = obter_df_diff_media_casos_interesse(df_vulnerabilidade = df_vulnerabilidade)

ranking_variaveis_por_media_t = obter_df_teste_t_casos_interesse(df_vulnerabilidade = df_vulnerabilidade)

list_tabelas_output[["RankingVariaveisMedia"]] <- ranking_variaveis_por_media

list_tabelas_output[["RankingVariaveisMediaTesteT"]] <- ranking_variaveis_por_media_t


View(ranking_variaveis_por_media)

View(ranking_variaveis_por_media_t)


ranking_variaveis_por_media = droplevels(as.data.frame(ranking_variaveis_por_media))

ranking_variaveis_por_media$Variavel = droplevels(ranking_variaveis_por_media$Variavel)


# Plotando Primeiras Variáveis da Diferença entre Médias

plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, variavel = "aPerfSlope", nome_amigavel_var = "aPerfSlope")

resultados_ranking_media = list(
  ranking_variaveis_por_media = ranking_variaveis_por_media
)

plots_ranking_media = list(
  plot_violino_1 = plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                             variavel = as.character(ranking_variaveis_por_media$Variavel[1]), 
                                                             nome_amigavel_var = as.character(ranking_variaveis_por_media$Variavel[1]))
  
  ,plot_violino_2 = plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                              variavel = as.character(ranking_variaveis_por_media$Variavel[2]), 
                                                              nome_amigavel_var = as.character(ranking_variaveis_por_media$Variavel[2]))
  
  ,plot_violino_3 = plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                              variavel = as.character(ranking_variaveis_por_media$Variavel[3]), 
                                                              nome_amigavel_var = as.character(ranking_variaveis_por_media$Variavel[3]))
  
  ,plot_violino_4 = plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                              variavel = as.character(ranking_variaveis_por_media$Variavel[4]), 
                                                              nome_amigavel_var = as.character(ranking_variaveis_por_media$Variavel[4]))
  
  ,plot_violino_5 = plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                              variavel = as.character(ranking_variaveis_por_media$Variavel[5]), 
                                                              nome_amigavel_var = as.character(ranking_variaveis_por_media$Variavel[5]))
  
  ,plot_violino_6= plot_violino_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                             variavel = as.character(ranking_variaveis_por_media$Variavel[6]), 
                                                             nome_amigavel_var = as.character(ranking_variaveis_por_media$Variavel[6]))
  
  ,plot_dispersao_media1 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                       variavel1 = as.character(ranking_variaveis_por_media$Variavel[1]), 
                                                                       nome_amigavel_var1 = as.character(ranking_variaveis_por_media$Variavel[1]),  
                                                                       variavel2 = as.character(ranking_variaveis_por_media$Variavel[2]), 
                                                                       nome_amigavel_var2 = as.character(ranking_variaveis_por_media$Variavel[2]))
  
  ,plot_dispersao_media2 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                       variavel1 = as.character(ranking_variaveis_por_media$Variavel[1]), 
                                                                       nome_amigavel_var1 = as.character(ranking_variaveis_por_media$Variavel[1]),  
                                                                       variavel2 = as.character(ranking_variaveis_por_media$Variavel[3]), 
                                                                       nome_amigavel_var2 = as.character(ranking_variaveis_por_media$Variavel[3]))
  
  ,plot_dispersao_media3 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                       variavel1 = as.character(ranking_variaveis_por_media$Variavel[2]), 
                                                                       nome_amigavel_var1 = as.character(ranking_variaveis_por_media$Variavel[2]),  
                                                                       variavel2 = as.character(ranking_variaveis_por_media$Variavel[3]), 
                                                                       nome_amigavel_var2 = as.character(ranking_variaveis_por_media$Variavel[3]))
)
  
plots_ranking_media$plot_violino_1
mapply(ggsave, file=paste0("./images/", names(plots_ranking_media), ".png"), plot=plots_ranking_media, width = plots_width, height = plots_heigh)





#### 4.3.1 - Feature Ranking - Outros Algoritmos ####

# Rodar Algoritmos para Priorização de Variáveis de Descoberta de Cenários.
# http://r-statistics.co/Variable-Selection-and-Importance-With-R.html
# Random Forest
set.seed(2)
# library(party)
# cf1 <- party::cforest(y ~ . , data= x, control=cforest_unbiased(mtry=2,ntree=50))
# importancia_party = varimp(cf1)
# varimp(cf1, conditional=TRUE)
# View(importancia_party)
# 
# caret::varImp(cf1)


# Usando uma Random forest "padrão"

forest_continuo = randomForest::randomForest(y_continuo~., data = x)

forest = randomForest::randomForest(factor(y)~., data = x)

# Logistic Regression
glm.fit <- glm(factor(y)~., data = x, family = binomial)
lm.fit = lm(formula = y_continuo~., data = x)



#### 4.3.2 Usando o model down e DALEX para observar os modelos

library(DALEX)

library(modelDown)

explainer_glm <- DALEX::explain(glm.fit, data=x, y= y)
explainer_lm <- DALEX::explain(lm.fit, data=x, y= y_continuo)
explainer_randomforest <- DALEX::explain(forest, data=x, y= y)
explainer_randomforest_continuo <- DALEX::explain(forest_continuo, data=x, y= y_continuo)


single_variable_pdp_rf = DALEX::single_variable(explainer_randomforest, "aReferencePopulation", type = "pdp")
single_variable_pdp_rf_continuo = DALEX::single_variable(explainer_randomforest_continuo, "aReferencePopulation", type = "pdp")
single_variable_pdp_glm = DALEX::single_variable(explainer_glm, "aReferencePopulation", type = "pdp")
single_variable_pdp_lm = DALEX::single_variable(explainer_lm, "aReferencePopulation", type = "pdp")



plot(single_variable_pdp_rf_continuo, single_variable_pdp_lm)


DALEX::model_performance(explainer_glm, explainer_randomforest)

# Gerando a Página com o ModelDown
modelDown::modelDown(explainer_lm, explainer_randomforest_continuo)

devtools::install_github("MI2DataLab/modelDown")
















#### 4.3.1 Visualizações Próprias

plot_importancia_forest = randomForest::varImpPlot(forest)

randomForest::varUsed(forest)


tabela_random_forest = as.data.frame(randomForest::importance(forest)) 

tabela_random_forest$Variavel = rownames(tabela_random_forest)

tabela_random_forest = tabela_random_forest[order(-tabela_random_forest$MeanDecreaseGini),]

tabela_random_forest$Rank = 1:nrow(tabela_random_forest)

tabela_random_forest = tabela_random_forest[,c(3,2,1)]

rownames(tabela_random_forest) = NULL

resultados_random_forest = list(
  tabela_random_forest = tabela_random_forest
)

list_tabelas_output[["RankingVariaveisRandomForest"]] <- tabela_random_forest

plots_random_forest = list(
  plot_dispersao_random_forest_1 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[1]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[1]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[2]))
  
  ,plot_dispersao_random_forest_2 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[1]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[1]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[3]))
  
  ,plot_dispersao_random_forest_3= plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[1]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[1]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[4]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[4]))
  
  ,plot_dispersao_random_forest_4= plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                               variavel1 = as.character(tabela_random_forest$Variavel[1]), 
                                                                               nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[1]),  
                                                                               variavel2 = as.character(tabela_random_forest$Variavel[5]), 
                                                                               nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[5]))
  
  ,plot_dispersao_random_forest_5= plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                               variavel1 = as.character(tabela_random_forest$Variavel[1]), 
                                                                               nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[1]),  
                                                                               variavel2 = as.character(tabela_random_forest$Variavel[6]), 
                                                                               nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[6]))
  
  ,plot_dispersao_random_forest_6 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                               variavel1 = as.character(tabela_random_forest$Variavel[1]), 
                                                                               nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[1]),  
                                                                               variavel2 = as.character(tabela_random_forest$Variavel[7]), 
                                                                               nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[7]))
  
  ,plot_dispersao_random_forest_7 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[3]))
  
  
  ,plot_dispersao_random_forest_8 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[4]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[4]))
  
  ,plot_dispersao_random_forest_9 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[5]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[5]))
  
  ,plot_dispersao_random_forest_10 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[6]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[6]))
  
  ,plot_dispersao_random_forest_11 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[7]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[7]))
  
  ,plot_dispersao_random_forest_12 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[3]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[4]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[4]))
  
  ,plot_dispersao_random_forest_13 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[3]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[5]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[5]))
  
  ,plot_dispersao_random_forest_14 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[3]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[6]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[6]))
  
  ,plot_dispersao_random_forest_15 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[3]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[7]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[7]))
  
  
  ,plot_dispersao_random_forest_16 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[4]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[4]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[5]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[5]))
  
  ,plot_dispersao_random_forest_17 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[4]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[4]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[6]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[6]))
  
  ,plot_dispersao_random_forest_18 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[4]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[4]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[7]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[7]))
  
  ,plot_dispersao_random_forest_19 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[5]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[5]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[6]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[6]))
  
  ,plot_dispersao_random_forest_20 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[5]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[5]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[7]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[7]))
  
  ,plot_dispersao_random_forest_21 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                 variavel1 = as.character(tabela_random_forest$Variavel[6]), 
                                                                                 nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[6]),  
                                                                                 variavel2 = as.character(tabela_random_forest$Variavel[7]), 
                                                                                 nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[7]))
  
)


mapply(ggsave, file=paste0("./images/", names(plots_random_forest), ".png"), plot=plots_random_forest, width = plots_width, height = plots_heigh)

variaveis_partial_plots = tabela_random_forest$Variavel[1:12]

# Primeira Forma de Gerar os Partial Plots - Gráficos Individuais:
for (v in variaveis_partial_plots) {
  message(paste("Gerando Partial Plot:", v))
  partial_df =  pdp::partial(forest, v, which.class = 2)
  partial_df = as.data.frame(partial_df)
  class(partial_df)
  names(partial_df) = c(v, paste0("yhat.",v))
  
  if(v == variaveis_partial_plots[1]) {
    list_partial_dependence_plots = list()
    df_completo_partial_plots = partial_df
  } else {
      df_completo_partial_plots = cbind(df_completo_partial_plots, partial_df)
  }
  
  list_partial_dependence_plots[[paste0("random_forest_pd_plot_",v)]] = plot_partial_plot(dados = df_completo_partial_plots, variavel = v, nome_amigavel_variavel = v)
  
}


mapply(ggsave, file=paste0("./images/", names(list_partial_dependence_plots), ".png"), plot=list_partial_dependence_plots, width = plots_width, height = plots_heigh)


plot_partial_dependence_geral = do.call("grid.arrange", c(list_partial_dependence_plots, ncol=3))


ggsave(filename = "./images/partial_dependence_plots_grid.png", plot = plot_partial_dependence_geral, width = plots_width, height = plots_width * 3/2)



# Two Variables
partial_plot_2_principais <- pdp::partial(forest, pred.var = c("aReferencePopulation", "aSwitchForCapacityStrategy2"), chull = TRUE, which.class = 2)


plot_partial_2_d <- autoplot(partial_plot_2_principais, contour = TRUE, 
                             legend.title = "Partial\ndependence")



# Segunda Forma, mais apropriada para gerar o gráfico Geral para que o eixo vertical fique igual
gerar_df_partial_plots = function(n_variavel, forest = forest) {
  v = variaveis_partial_plots[n_variavel]
  df_variavel = pdp::partial(forest, v, which.class = 2)
  df_variavel$Variavel = v
  names(df_variavel) = c("Incerteza", "PartialDependence", "Variavel")
  df_variavel
}

list_partial_dependence_df = lapply(X = 1:length(variaveis_partial_plots), FUN = gerar_df_partial_plots, forest = forest)

df_completo_partial_plots2 = do.call(rbind, list_partial_dependence_df)


names(df_completo_partial_plots2)


df_completo_partial_plots2$Variavel = factor(df_completo_partial_plots2$Variavel, levels = variaveis_partial_plots)

plot_partial_dependence_completo2 = plot_partial_plot_n_variaveis(dados = df_completo_partial_plots2)


ggsave(filename = "./images/partial_dependence_plots_grid2.png", plot = plot_partial_dependence_completo2, width = plots_width, height = plots_width * 3/2)





plot_dispersao_comparacao_random_forest = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                             variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                             nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                             variavel2 = as.character(tabela_random_forest$Variavel[1]), 
                                                                             nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[1]))

list_plots_comparacao = list(
  PlotRandomForest = plot_partial_2_d + ggtitle("Random Forest - PDP") + theme(legend.position = "bottom"),
  PlotDispersao = plot_dispersao_comparacao_random_forest + ggtitle("Dados Simulados")
)


plot_comparacao_partial_dependence = do.call("grid.arrange", c(list_plots_comparacao, ncol=2))

ggsave(filename = "./images/partial_dependence_comparacao_dados.png", plot = plot_comparacao_partial_dependence, width = plots_width, height = plots_heigh)




## Partial Dependence Plots com a biblioteca DALEX

devtools::install_github("pbiecek/DALEX")
library(DALEX)

explainer_rf = DALEX::explain(model = forest, data = x)

expl_rf = single_variable(explainer = explainer_rf, variable = "aReferencePopulation", type = "pdp")

plot(expl_rf)

# Usando as Variáveis com Shortlist, o resultado funcionou.
# library(relaimpo)
# lmMod <- lm(y ~ . , data = x)
# relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)
# calc.relimp(lmMod, rela = TRUE)
# # calculate relative importance scaled to 100
# sort(relImportance$lmg, decreasing=TRUE)  # rel

# por algum motivo o discount Rate não está lá dentro:

# library(breakDown)
# br = broken(lmMod, new_observation = ensemble_e_resultados[18,])
# plot(br)


### O resultado desta análise fez um pouco de sentido (e não é óbvio).
# library(earth)
# marsModel <- earth(y ~ ., data= x) 
# ev <- evimp(marsModel)
# plot(ev)


# Stepwise (Também faz sentido, quando a variável não é considerada um fator, como é o caso).
# base.mod <- lm(y ~ 1 , data= x)  # base intercept only model
# all.mod <- lm(y ~ . , data= x) # full model with all predictors
# stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
# shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
# shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
# print(shortlistedVars)


# Rodando o Algoritmo Boruta
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(y ~ ., data=na.omit(x), doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif)
plot_boruta = plot(boruta_output, cex.axis=.5, las=3, xlab="", main="Variable Importance")  # plot variable importance

plot_boruta

plot_boruta = plot(boruta_output, cex.axis=.7, las=2, mar=c(5,8,4,2), xlab="", main="Variable Importance")  # plot variable importance

parametros_selecionados_boruta = getSelectedAttributes(boruta_output)

tabela_resultados_boruta = arrange(cbind(attr=rownames(attStats(boruta_output)), attStats(boruta_output)),desc(medianImp))

variavel_1 = boruta_signif[1]
variavel_2 = boruta_signif[2]


resultados_boruta = list(
  plot_boruta = plot_boruta,
  tabela_resultados_boruta = tabela_resultados_boruta
)

list_tabelas_output[["RankingVariaveisBoruta"]] <- tabela_resultados_boruta



ranking_unificado = data.frame(
  Posicao = 1:nrow(ranking_variaveis_por_media),
  VariaveisRankingRandomForest = resultados_random_forest$tabela_random_forest$Variavel,
  ImportanciaRandomForest = resultados_random_forest$tabela_random_forest$MeanDecreaseGini,
  VariaveisRankingBoruta = resultados_boruta$tabela_resultados_boruta$attr,
  ImportanciaMediaBoruta = resultados_boruta$tabela_resultados_boruta$meanImp,
  DecisaoBoruta = resultados_boruta$tabela_resultados_boruta$decision,
  VariaveisRankingMedia = ranking_variaveis_por_media$Variavel,
  DiferencaMediaRelativa = ranking_variaveis_por_media$DifMediaRelativa,
  VariaveisRankingMediaT = ranking_variaveis_por_media_t$Variável,
  ValorP = ranking_variaveis_por_media_t$Valor_P
)

View(ranking_unificado)

list_tabelas_output[["RankingGeral"]] <- ranking_unificado






#### 4.3.2 - PRIM ####


# Antes de Rodar o PRIM, posso selecionar as primeiras três variáveis de todos os Rankings:
n_variaveis_rankings = 5
primeiras_variaveis_media = as.character(ranking_variaveis_por_media$Variavel[1:n_variaveis_rankings])
primeiras_variaveis_teste_t = as.character(ranking_variaveis_por_media_t$Variável[1:n_variaveis_rankings])
primeiras_variaveis_random_forest = as.character(resultados_random_forest$tabela_random_forest$Variavel[1:n_variaveis_rankings])
primeiras_variaveis_boruta = as.character(resultados_boruta$tabela_resultados_boruta$attr[1:n_variaveis_rankings])


variaveis_shortlist = unique(c(primeiras_variaveis_media, primeiras_variaveis_boruta, primeiras_variaveis_random_forest, primeiras_variaveis_teste_t))


#### Plot Estratégias Versus Incertezas ####

plot_grafico_shortlist_5 = plot_estrategias_versus_incertezas(df_vulnerabilidade = df_vulnerabilidade,incertezas = variaveis_shortlist[1:5], binario = TRUE)

plot_grafico_shortlist_5



# Rodando a Análise do PRIM no python (chamando por aqui.)
# Edite o script do python para que isto funcione corretamente.

y = df_vulnerabilidade$sNPVProfit1Regret
x = df_vulnerabilidade[,variaveis_shortlist]
x_completo = df_vulnerabilidade[,5:length(names(df_vulnerabilidade))]
# Preparando Variáveis Categóricas para o PRIM:

# A Aopção abaixo pega todas as variáveis de incerteza:
#x = df_vulnerabilidade[,5:ncol(df_vulnerabilidade)]

write.csv(y, file = "resposta.csv")

write.csv(x, file = "incertezas.csv")

write.csv(x_completo, file = "incertezas_completas.csv")

View(y)
View(x)

threshold_analise_vulnerabilidade

# Realizar a Análise no Python.


#### 4.3.4 Comparando Estratégias do Ranking ####

## Gerar Objeto com Ensemble e Resultados:

# Obter Ensemble com Dados Simulados (com todas as análises).
ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")

# Retirar NAs do Ensemble
ensemble_e_resultados = na.omit(ensemble_e_resultados)


# Utilizar apenas as primeiras estratégias do ranking:

top_10_estrategias = ranking_estrategias$Lever[1:12]


# Filtrar o Ensemble e Resultados para mostrar estratégias nos top 10
ensemble_e_resultados = subset(ensemble_e_resultados, Lever %in% top_10_estrategias)


variaveis_incerteza = names(df_vulnerabilidade[,5:length(names(df_vulnerabilidade))])

names(results$DadosUltimoPeriodo)

variaveis_desejadas = c(variaveis_incerteza,
                          c("sPrice1", "sCumulativeAdopters", "sCumulativeProduction1", "sPatentLefts", "fIndustryOrderRate", "aPerformance1", "aIndustryShipments", "sInstalledBase1", "sBacklog1", "sInstalledBase2", "sInstalledBase4", "sPrice2", "sPrice4", "aOrderShare1", "aOrderShare2","aOrderShare3", "aOrderShare4"))


list_plots_estrategias_incerteza_profit_regret = list()

for (v in variaveis_desejadas) {
  message(paste("Gerando Plot:", v))
  
  list_plots_estrategias_incerteza_profit_regret[[paste0("estrategia_vs_profit_regret_",v)]] = plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                                                                                                           variavel1 = v, 
                                                                                                           nome_amigavel_var1 = v, 
                                                                                                           variavel2 = "sNPVProfit1Regret", 
                                                                                                           nome_amigavel_var2 = "Custo de Oportunidade")
}

list_plots_estrategias_incerteza_profit_regret$estrategia_vs_profit_regret_aFractionalDiscardRate

mapply(ggsave, file=paste0("./images/", names(list_plots_estrategias_incerteza_profit_regret), ".png"), plot=list_plots_estrategias_incerteza_profit_regret, width = plots_width, height = plots_width)


list_plots_estrategias_incerteza_profit = list()

for (v in variaveis_desejadas) {
  message(paste("Gerando Plot:", v))
  
  list_plots_estrategias_incerteza_profit[[paste0("estrategia_vs_profit_",v)]] = plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                                                                                                                             variavel1 = v, 
                                                                                                                             nome_amigavel_var1 = v, 
                                                                                                                             variavel2 = "sNPVProfit1", 
                                                                                                                             nome_amigavel_var2 = "VPL")
}


list_plots_estrategias_incerteza_profit$estrategia_vs_profit_aReferencePopulation

mapply(ggsave, file=paste0("./images/", names(list_plots_estrategias_incerteza_profit), ".png"), plot=list_plots_estrategias_incerteza_profit, width = plots_width, height = plots_width)




plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% c(31, 19, 25)),
                              variavel1 = "sNPVProfit1", 
                              nome_amigavel_var1 = "VPL", 
                              variavel2 = "sPrice1", 
                              nome_amigavel_var2 = "Preço Player 1")




plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% c(1, 2)),
                              variavel1 = "sPrice1", 
                              nome_amigavel_var1 = "Preço Player 1", 
                              variavel2 = "aPerformance1", 
                              nome_amigavel_var2 = "Perf. Player 1", linha_regr = FALSE, facet = FALSE)



plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% c(31, 44)),
                              variavel1 = "sPrice1", 
                              nome_amigavel_var1 = "Preço Player 1", 
                              variavel2 = "sInstalledBase1", 
                              nome_amigavel_var2 = "Imp. 3D Installadas Player 1", linha_regr = FALSE, facet = FALSE)


duas_estrategias_a_comparar = c(31,32)

list_plots_duas_estrategias = list(
  plot_duas_estrategias_preco_base_instalada = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                             variavel1 = "sPrice1", 
                                                                             nome_amigavel_var1 = "Player 1 Avg. Price", 
                                                                             variavel2 = "sInstalledBase1", 
                                                                             nome_amigavel_var2 = "Player 1 Installed Base", linha_regr = FALSE, facet = FALSE) + scale_color_hue(labels = c("31 - Agress.", "32 - Conserv."))
  
  ,plot_duas_estrategias_preco_market_share = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                           variavel1 = "sPrice1", 
                                                                           nome_amigavel_var1 = "Player 1 Avg. Price", 
                                                                           variavel2 = "aOrderShare1", 
                                                                           nome_amigavel_var2 = " Player 1 Market Share", linha_regr = FALSE, facet = FALSE)  + scale_color_hue(labels = c("31 - Agress.", "32 - Conserv."))
  
  ,plot_duas_estrategias_preco_vpl = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                  variavel1 = "sPrice1", 
                                                                  nome_amigavel_var1 = "Player 1 Avg. Price", 
                                                                  variavel2 = "sNPVProfit1", 
                                                                  nome_amigavel_var2 = "Player 1 NPV", linha_regr = FALSE, facet = FALSE)  + scale_color_hue(labels = c("31 - Agressive", "32 - Conservative"))
  
  
)



plot_comparacao_duas_estrategias = do.call("grid.arrange", c(list_plots_duas_estrategias, ncol=3))


do.call("grid.arrange", c(list_plots_duas_estrategias, ncol=3))



duas_estrategias_a_comparar = c(31,25)

variavel_a_comparar_x = paste0("sPrice",1:N_PLAYERS)
variavel_a_comparar_y = paste0("sNPVProfit",1:N_PLAYERS)

list_plots_duas_estrategias_players = list(
  plot_duas_estrategias_players_1 = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                             variavel1 = variavel_a_comparar_x[1], 
                                                                             nome_amigavel_var1 = variavel_a_comparar_x[1], 
                                                                             variavel2 = variavel_a_comparar_y[1], 
                                                                             nome_amigavel_var2 = variavel_a_comparar_y[1], linha_regr = FALSE, facet = FALSE)
  
  
  ,plot_duas_estrategias_players_2 = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                  variavel1 = variavel_a_comparar_x[2], 
                                                                  nome_amigavel_var1 = variavel_a_comparar_x[2], 
                                                                  variavel2 = variavel_a_comparar_y[2], 
                                                                  nome_amigavel_var2 = variavel_a_comparar_y[2], linha_regr = FALSE, facet = FALSE)
  
  
  ,plot_duas_estrategias_players_3 = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                   variavel1 = variavel_a_comparar_x[3], 
                                                                   nome_amigavel_var1 = variavel_a_comparar_x[3], 
                                                                   variavel2 = variavel_a_comparar_y[3], 
                                                                   nome_amigavel_var2 = variavel_a_comparar_y[3], linha_regr = FALSE, facet = FALSE)
  
  
  ,plot_duas_estrategias_players_4 = plot_dispersao_duas_variaveis(df_dados = subset(ensemble_e_resultados, Lever %in% duas_estrategias_a_comparar),
                                                                   variavel1 = variavel_a_comparar_x[4], 
                                                                   nome_amigavel_var1 = variavel_a_comparar_x[4], 
                                                                   variavel2 = variavel_a_comparar_y[4], 
                                                                   nome_amigavel_var2 = variavel_a_comparar_y[4], linha_regr = FALSE, facet = FALSE)
  
  
  
  
)



plot_comparacao_duas_estrategias_player_mesmas_variaveis = do.call("grid.arrange", c(list_plots_duas_estrategias_players, ncol=2))












plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = as.character(ranking_variaveis_por_media$Variavel[2]), 
                              nome_amigavel_var1 = as.character(ranking_variaveis_por_media$Variavel[2]), 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")



plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "aPerfSlope", 
                              nome_amigavel_var1 = "aPerfSlope", 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")



plot_dispersao_duas_variaveis_cor(df_dados = ensemble_e_resultados,
                                  variavel_cor = "CasoInteresse",
                              variavel1 = "aPerfSlope", 
                              nome_amigavel_var1 = "aPerfSlope", 
                              variavel2 = "aReferencePopulation", 
                              nome_amigavel_var2 = "aReferencePopulation")






plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "aPerfSlope", 
                              nome_amigavel_var1 = "aPerfSlope", 
                              variavel2 = "sNPVProfit1", 
                              nome_amigavel_var2 = "VPL Pl 1")




plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = as.character(ranking_variaveis_por_media$Variavel[2]), 
                              nome_amigavel_var1 = as.character(ranking_variaveis_por_media$Variavel[2]), 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")





plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = ranking_variaveis_por_media$Variavel[2], 
                              nome_amigavel_var1 = ranking_variaveis_por_media$Variavel[2], 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")


plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = ranking_variaveis_por_media$Variavel[3], 
                              nome_amigavel_var1 = ranking_variaveis_por_media$Variavel[3], 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")







plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "aReferencePopulation", 
                              nome_amigavel_var1 = "Mercado Estimado", 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")


plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "sNPVProfit1", 
                              nome_amigavel_var1 = "VPL Player 1", 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade Player 1")



plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "aInitialsPatentesEmDominioPublicoUteis", 
                              nome_amigavel_var1 = "Patentes em dom. publ. úteis", 
                              variavel2 = "sNPVProfit1", 
                              nome_amigavel_var2 = "VPL Player 1")





plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "aInitialsPatentesEmDominioPublicoUteis", 
                              nome_amigavel_var1 = "Patentes em dom. publ. úteis", 
                              variavel2 = "sNPVProfit1", 
                              nome_amigavel_var2 = "VPL Player 1")






plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = "fInvestimentoPeD1", 
                              nome_amigavel_var1 = "Investimento em P & D", 
                              variavel2 = "aPatentesEmpresaTemAcesso1", 
                              nome_amigavel_var2 = "Patentes Acessadas pelo Player 1")


plot_dispersao_duas_variaveis(df_dados = results$DadosUltimoPeriodo,
                              variavel1 = "aPatentesEmpresaTemAcesso1", 
                              nome_amigavel_var1 = "Patentes Acessadas pelo Player 1", 
                              variavel2 = "aPerformance1", 
                              nome_amigavel_var2 = "Performance Player 1")


plot_dispersao_duas_variaveis(df_dados = results$DadosUltimoPeriodo,
                              variavel1 = "aPerformance1", 
                              nome_amigavel_var1 = "Performance Player 1", 
                              variavel2 = "aAttractivenessFromPerformance1", 
                              nome_amigavel_var2 = "Atrativ. ")


plot_dispersao_duas_variaveis(df_dados = results$DadosUltimoPeriodo,
                              variavel1 = "fInvestimentoPeD1", 
                              nome_amigavel_var1 = "fInvestimentoPeD1", 
                              variavel2 = "aAttractivenessFromPerformance1", 
                              nome_amigavel_var2 = "aAttractivenessFromPerformance1")

plot_dispersao_duas_variaveis(df_dados = results$DadosUltimoPeriodo,
                              variavel1 = "fInvestimentoPeD1", 
                              nome_amigavel_var1 = "fInvestimentoPeD1", 
                              variavel2 = "aAttractivenessFromPrice1", 
                              nome_amigavel_var2 = "aAttractivenessFromPrice1")




plot_dispersao_duas_variaveis(df_dados = results$DadosUltimoPeriodo,
                              variavel1 = "fInvestimentoPeD1", 
                              nome_amigavel_var1 = "fInvestimentoPeD1", 
                              variavel2 = "aOrderShare1", 
                              nome_amigavel_var2 = "aOrderShare1")




plot_dispersao_duas_variaveis(df_dados = results$AnaliseRegret$Dados,
                              variavel1 = "sNPVProfit1Regret", 
                              nome_amigavel_var1 = "sNPVProfit1Regret", 
                              variavel2 = "sNPVProfit1", 
                              nome_amigavel_var2 = "sNPVProfit1")








#### 4.4 Análise de Tradeoffs ####



resultados_analise_tradeoff = plot_fronteira_tradeoff_estrategia(results = results, opcoes = opcoes)

resultados_analise_tradeoff$PlotTradeoffOdds

resultados_analise_tradeoff$PlotTradeoffDispersao

ggsave(filename = "plot_tradeoff_dispersao.png", plot = resultados_analise_tradeoff$PlotTradeoffDispersao, width = plots_width, height = plots_heigh, path = "./images/")

ggsave(filename = "plot_tradeoff_curva_odds.png", plot = resultados_analise_tradeoff$PlotTradeoffOdds, width = plots_width, height = plots_heigh, path = "./images/")

mapply(ggsave, file=paste0("./images/", names(resultados_analise_tradeoff), ".png"), plot=resultados_analise_tradeoff, width = plots_width, height = plots_heigh)


plot_tradeoff_regret_vpl(results = results, opcoes = opcoes)


# results.prim = prim.box(x = x, y = y, threshold.type = 1, peel.alpha = 0.25, paste.alpha = 0.15, threshold = 0.3)


summary(results.prim, print.box = TRUE)

plot(results.prim, splom=FALSE)

results.prim

plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, variavel1 = "aSwitchForCapacityStrategy2", variavel2 = "aDesiredMarketShare4", nome_amigavel_var1 = "aSwitchForCapacityStrategy2", nome_amigavel_var2 = "aDesiredMarketShare4")

plot(results.prim, col="transparent")

plot(results.prim)
points(x)

library(subgroup.discovery)




#### Gráfico de superfície:

gerar_grafico_superficie(dados_ultimo_ano = df_vulnerabilidade, variaveis = c("aSwitchForCapacityStrategy2", "aReferencePopulation", "sNPVProfit1Regret"), estrategia = 31)


#### Gerando Tabelas no Excel ####




library(openxlsx)
openxlsx::write.xlsx(x = list_tabelas_output, 
                     file = "tabelas_analise.xlsx")



#### Dados de Fundamentos ####
dados_fundamentos = obter_dados_fundamentos_us_fundamentals()




   + xlab(label = "Tempo") + ylab(label = "Valor da Ação - Fechamento")


scale_x_date(format = "%b-%Y")





plot_lucro_bruto_us_fundamentals = ggplot(dados_fundamentos, aes(x=Ano, y=GrossProfit, group=empresa)) +
  geom_line(aes(color=empresa))+
  geom_point(aes(color=empresa)) +
  ylab(label = "Lucro Bruto") +
  scale_y_continuous(labels = format_for_humans)

plot_ped_us_fundamentals = ggplot(dados_fundamentos, aes(x=Ano, y=ResearchAndDevelopmentExpense, group=empresa)) +
  geom_line(aes(color=empresa))+
  geom_point(aes(color=empresa)) +
  ylab(label = "OPEX P&D") +
  scale_y_continuous(labels = format_for_humans)


plot_lucro_bruto_us_fundamentals

fundamentos_ddd = obter_fundamentos_financeiros_quandl("DDD")

fundamentos_mtls = obter_fundamentos_financeiros_quandl("PRLB")

plot_receita_investimento_3dsystems = plot_linha_duas_variaveis(fundamentos_ddd$Dados, variavel1 = "Revenue", nome_amigavel_variavel1 = "Receita", variavel2 = "ResearchAndDevelopmentExpenses", nome_amigavel_variavel2 = "OPEX P & D")


plot_receita_ebitda = plot_linha_duas_variaveis(fundamentos_ddd$Dados, variavel1 = "Revenue", nome_amigavel_variavel1 = "Revenue", variavel2 = "NetCashFlowFromOperations", nome_amigavel_variavel2 = "Net CF from Operations")


plot_cash_net_income_3dsystems = plot_linha_duas_variaveis(fundamentos_ddd$Dados, variavel1 = "NetIncome", nome_amigavel_variavel1 = "Lucro Líquido", variavel2 = "GrossProfit", nome_amigavel_variavel2 = "Lucro Bruto")

fundamentos_3DSYSTEMS = obter_fundamentos_financeiros_quandl(company_code = "DDD")

fundamentos_GE = obter_fundamentos_financeiros_quandl(company_code = "GE")

fundamentos_GE$Dados$OrcamentoPeD = fundamentos_GE$Dados$ResearchAndDevelopmentExpenses / fundamentos_GE$Dados$Revenue

fundamentos_3DSYSTEMS$Dados$OrcamentoPeD = fundamentos_3DSYSTEMS$Dados$ResearchAndDevelopmentExpenses / fundamentos_3DSYSTEMS$Dados$Revenue

plot_receita_investimento_GE = plot_linha_duas_variaveis(fundamentos_GE$Dados, variavel1 = "Revenue", nome_amigavel_variavel1 = "Receita", variavel2 = "ResearchAndDevelopmentExpenses", nome_amigavel_variavel2 = "OPEX P & D")

plot_receita_investimento_3dsystems = plot_linha_duas_variaveis(fundamentos_3DSYSTEMS$Dados, variavel1 = "Revenue", nome_amigavel_variavel1 = "Receita", variavel2 = "ResearchAndDevelopmentExpenses", nome_amigavel_variavel2 = "Despesas com P & D")

plot_orcamentoPeD__3dsystems = plot_linha_uma_variavel(fundamentos_3DSYSTEMS$Dados, variavel = "OrcamentoPeD", nome_amigavel_variavel = "OrçamentoPeD / Receita")


# Despesas em Pesquisa e Desenvolvimento da 3D Systems em relação a PeD
plot_orcamento_PeD_3DSystems = ggplot(DDD.orcamentoPeD)





# Obter dados de Estoque.

Quandl.api_key("RsCuvs4_WjRPP_zzSzfv")

teste_preco_stock_3D_systems = Quandl("WIKI/DDD", collapse="monthly", start_date="1900-01-01", type="ts")


df_stocks_DDD = data.frame(time = as.vector(time(teste_preco_stock_3D_systems)),
                           as.data.frame(teste_preco_stock_3D_systems))


teste_preco_stock_Stratasys = Quandl("WIKI/SSYS", collapse="monthly", start_date="1900-01-01", type="ts")

df_stocks_SSYS = data.frame(time = as.vector(time(teste_preco_stock_Stratasys)),
                            as.data.frame(teste_preco_stock_Stratasys))



join_acoes = dplyr::inner_join(df_stocks_DDD, df_stocks_SSYS, by = "time")


plot_acao_3D_Systems = ggplot(df_stocks_DDD, aes(time, Close)) + geom_line() + xlab("Tempo") + ylab("Valor da Ação - DDD")


plot_acao_Stratasys = ggplot(df_stocks_SSYS, aes(time, Close)) + geom_line() + xlab("Tempo") + ylab("Valor da Ação - SSYS")

library(gapminder)
library(gganimate)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


gganimate::anim_save()

#### Gerando a Apresentação ####

rmarkdown::render("dmdu-presentation/dmdu-presentation.Rmd", encoding = "UTF-8")

