####Configurando Simulações ####
# Carregando Funções Úteis
START<-2007; FINISH <-2017; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')
# Parâmetros para a Geração dos Gráficos
plots_width = 7
plots_heigh = 4

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
  ModoParalelo = "FORK",
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

n_casos_total = 54 * 5 # 400

n_estrategias = nrow(carregar_inputs(arquivo_de_inputs = planilha_simulacao_opcao1_futuro, opcoes = opcoes)$Levers)

# Tamanho do Ensemble Adimitido (para simular todas as estratégias)
n_ensemble_total = round(n_casos_total / n_estrategias, 0) 

# Tamanho do ensemble para calibração.
n_ensemble_calibracao = round(n_ensemble_total / percentil_utilizado_como_criterio,0)


####Verificação - Comparação de Simulações com o Ithink ####
# Rodando um Cenário Base
## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
opcoes$FullFactorialDesign = FALSE
opcoes$Paralelo = FALSE
START<-0; FINISH<-10; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = TRUE; VERIFICAR_CHECKS = TRUE; CHECK_PRECISION = 0.1; BROWSE_ON_DIFF = TRUE
VERIFICAR_GLOBAL = TRUE;
## Carregando Modelo
source('funcoes.R', encoding = 'UTF-8')

# Carregando Variáveis de Output do Ithink para Comparação
arquivo_excel_stocks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_stocks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
arquivo_excel_checks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_checks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
arquivo_excel_global = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_tudo.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)


dados_ithink_stocks  = arquivo_excel_stocks$ResultadosIthink %>% dplyr::select(-Months)
dados_ithink_checks  = arquivo_excel_checks$ResultadosIthink %>% dplyr::select(-Months)

dados_ithink_global = arquivo_excel_global$ResultadosIthink %>% dplyr::select(-Months)


variaveis_ithink_stocks = names(dados_ithink_stocks)
variaveis_ithink_checks = names(dados_ithink_checks)

variaveis_globais_a_verificar = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/variaveis_globais_a_verificar.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"), opcoes = opcoes)
variaveis_globais_a_verificar = as.vector(variaveis_globais_a_verificar$ResultadosIthink$variaveis)

parametros_completos = readxl::read_xlsx(planilha_simulacao_calibracao_historico, sheet = "params_testeithink")

parametros_cenariobase = t(parametros_completos[,"CenarioBase"])[1,]

names(parametros_cenariobase) = as.matrix(parametros_completos[,1])


resultados_caso_base = solve_modelo_dissertacao(parametros = parametros_cenariobase, modelo = sdmodel$Modelo, simtime = sdmodel$SimTime)


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

# Salvar resultados com casos plausíveis:
#save(resultados_casos_plausiveis, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/resultados_casos_plausiveis.rda")


save(resultados_casos_plausiveis, file = "resultados_casos_plausiveis.rda")


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

variaveis_analise_fit = c("SumOfSquareResiduals", "MeanSquareError", "MeanAbsoluteError", "MeanAbsolutePercentError", "UM_ThielBiasDiffMeans", "US_ThielUnequalVariation", "UC_ThielUnequalCovariation")
variaveis_exibir_ensemble = c("Scenario", variaveis_analise_fit)
cenarios_a_exibir_tabela = sample(1:opcoes$N,size = 5)

# Demonstração do erro calculado

tabela_de_erro_calculado = t(ensemble_com_erro[1:5,variaveis_exibir_ensemble])

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
  ylab("Demanda Total")+
  xlab("Anos")+
  scale_y_continuous(labels = format_for_humans)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Dados",
                               "Modelo"))
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
results = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                            sdmodel = sdmodel, 
                                            opcoes = opcoes)

# Salvar Resultados com apenas 10 anos simulados.
save(results, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/results_final.rda")

#### 4.3 Análise dos Resultados ####

# Escolher Estratégia Candidata de acordo com a definição de Critério:
results$EstrategiaCandidata = escolher_estrategia_candidata(dados = results$AnaliseRegret$Dados, resumo_estrategias = results$AnaliseRegret$ResumoEstrategias, var_resposta = opcoes$VarResposta, var_criterio = opcoes$VarCriterio, sentido = opcoes$SentidoCriterio)


# Gerar Gráficos para Analisar os Resultados:

# Gerando Resultados da Opção 1 - Estratégia Candidata 1, e Cenário 1
plots_results = salvar_plots_result(results = results, 
                                    cenario_plot_players = results$DadosUltimoPeriodo$Scenario[1],
                                    estrategia_candidata = results$EstrategiaCandidata$Lever[1],
                                    opcoes = opcoes)

plots_results$plots_linha_geral$plot_estrategia_candidata_demanda_global

plots_results$plots_whisker$plot_whisker_lever_regret

plots_results$plots_whisker$plot_whisker_lever_profit

plots_results$plots_whisker$plot_whisker_lever_share

grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aPerformance1")


grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aOrderShare4")

grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aOrderShare3")

grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aOrderShare2")

grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aOrderShare1")

grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aPatentesEmpresaTemAcesso1")


grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sPrice1")


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
x = df_vulnerabilidade[,5:ncol(df_vulnerabilidade)]


#### 4.3.1 - Ranking de Variáveis considerando apenas médias.####
ranking_variaveis_por_media = obter_df_diff_media_casos_interesse(df_vulnerabilidade = df_vulnerabilidade)

ranking_variaveis_por_media

View(ranking_variaveis_por_media)


ranking_variaveis_por_media = droplevels(as.data.frame(ranking_variaveis_por_media))

ranking_variaveis_por_media$Variavel = droplevels(ranking_variaveis_por_media$Variavel)


# Plotando Primeiras Variáveis da Diferença entre Médias

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
library(randomForest)
forest = randomForest::randomForest(factor(y)~., data = x)
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
  
  ,plot_dispersao_random_forest_3 = plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, 
                                                                                variavel1 = as.character(tabela_random_forest$Variavel[2]), 
                                                                                nome_amigavel_var1 = as.character(tabela_random_forest$Variavel[2]),  
                                                                                variavel2 = as.character(tabela_random_forest$Variavel[3]), 
                                                                                nome_amigavel_var2 = as.character(tabela_random_forest$Variavel[3]))
)


mapply(ggsave, file=paste0("./images/", names(plots_random_forest), ".png"), plot=plots_random_forest, width = plots_width, height = plots_heigh)





library(randomForestExplainer)
library(pdp)


pdp1.1 = pdp::partial(forest, as.character(tabela_random_forest$Variavel[1]))
plot_random_forest_1d.1 = pdp::plotPartial(pdp1.1)

plot_random_forest_1d.1


pdp1.2 = pdp::partial(forest, as.character(tabela_random_forest$Variavel[2]))
plot_random_forest_1d.2 = pdp::plotPartial(pdp1.2)

plot_random_forest_1d.2


pdp1.3 = pdp::partial(forest, as.character(tabela_random_forest$Variavel[3]))
plot_random_forest_1d.3 = pdp::plotPartial(pdp1.3)

plot_random_forest_1d.3



pdp2 = pdp::partial(forest,  as.character(tabela_random_forest$Variavel[1:2]))

plot_parcial2d = pdp::plotPartial(pdp2)

plot_parcial2d

estrategia_candidata = results$EstrategiaCandidata

variavel_resposta = "sNPVProfit1Regret"


landscape_estrategia_comparacao = plot_landscape_futuros_plausiveis(
  results, estrategia = results$EstrategiaCandidata$Lever, 
  variavelresp = variavel_resposta,
  nomeamigavel_variavelresp = "Custo de Oportunidade",
  variavel1  = as.character(tabela_random_forest$Variavel[1]),
  n_variavel1 = as.character(tabela_random_forest$Variavel[1]),
  variavel2 = as.character(tabela_random_forest$Variavel[2]),
  n_variavel2 = as.character(tabela_random_forest$Variavel[2])
)



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



# Antes de Rodar o PRIM, posso selecionar as primeiras três variáveis de todos os Rankings:
n_variaveis_rankings = 5
primeiras_variaveis_media = as.character(ranking_variaveis_por_media$Variavel[1:n_variaveis_rankings])
primeiras_variaveis_random_forest = as.character(resultados_random_forest$tabela_random_forest$Variavel[1:n_variaveis_rankings])
primeiras_variaveis_boruta = as.character(resultados_boruta$tabela_resultados_boruta$attr[1:n_variaveis_rankings])


variaveis_shortlist = unique(c(primeiras_variaveis_media, primeiras_variaveis_boruta, primeiras_variaveis_random_forest))

#### 4.3.2 - PRIM ####

# Rodando a Análise do PRIM no python (chamando por aqui.)
# Edite o script do python para que isto funcione corretamente.

y = df_vulnerabilidade$sNPVProfit1Regret
x = df_vulnerabilidade[,variaveis_shortlist]
# A Aopção abaixo pega todas as variáveis de incerteza:
#x = df_vulnerabilidade[,5:ncol(df_vulnerabilidade)]

write.csv(y, file = "resposta.csv")

write.csv(x, file = "incertezas.csv")

View(y)
View(x)

threshold_analise_vulnerabilidade


#### 4.3.4 Comparando Estratégias do Ranking ####


## Gerar Objeto com Ensemble e Resultados:

# Obter Ensemble com Dados Simulados (com todas as análises).
ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")

# Retirar NAs do Ensemble
ensemble_e_resultados = na.omit(ensemble_e_resultados)

# Utilizar apenas as primeiros 8 estratégias do ranking:

top_10_estrategias = ranking_estrategias$Lever[1:6]

# Filtrar o Ensemble e Resultados para mostrar estratégias nos top 10
ensemble_e_resultados = subset(ensemble_e_resultados, Lever %in% top_10_estrategias)


plot_dispersao_duas_variaveis(df_dados = ensemble_e_resultados,
                              variavel1 = as.character(ranking_variaveis_por_media$Variavel[1]), 
                              nome_amigavel_var1 = as.character(ranking_variaveis_por_media$Variavel[1]), 
                              variavel2 = "sNPVProfit1Regret", 
                              nome_amigavel_var2 = "Custo de Oportunidade")

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



plot_fronteira_tradeoff_estrategia(results = results, opcoes = opcoes)


# results.prim = prim.box(x = x, y = y, threshold.type = 1, peel.alpha = 0.25, paste.alpha = 0.15, threshold = 0.3)


summary(results.prim, print.box = TRUE)

plot(results.prim, splom=FALSE)

results.prim

plot_dispersao_casos_interesse_por_variavel(df_vulnerabilidade = df_vulnerabilidade, variavel1 = "aSwitchForCapacityStrategy2", variavel2 = "aDesiredMarketShare4", nome_amigavel_var1 = "aSwitchForCapacityStrategy2", nome_amigavel_var2 = "aDesiredMarketShare4")

plot(results.prim, col="transparent")

plot(results.prim)
points(x)

library(subgroup.discovery)





#### Dados de Fundamentos ####
dados_fundamentos = obter_dados_fundamentos_us_fundamentals()



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


#### Dados de Patentes ####

# Esta busca de patentes por nome do proprietário da patente não foi produtiva. Existem casos onde a ferramenta não encontra patentes (ex.:  3D Systems).

library(patentsview)

patentsview::get_endpoints()

patentsview::get_fields(endpoint = "assignees")

qry_3 = qry_funs$text_phrase(patent_title = c("simulated leather", "leather-like", "artificial leather", "synthetic leather"))

query_assignee = qry_funs$text_phrase(assignee_organization = "3D")
teste_patentes = search_pv(query = query_assignee, fields = c("patent_id", "patent_number", "patent_title", "patent_year", "patent_type"), all_pages = TRUE)


query_assignee = qry_funs$contains(assignee_first_name = "3D")


qry_4 = qry_funs$text_phrase(patent_title = c("simulated leather", "leather-like", "artificial leather", "synthetic leather"))

# Get all patent and assignee-level fields for the patents endpoint:
fields <- get_fields(endpoint = "patents",
                     groups = c("assignees", "patents"))


View(patentsview::fieldsdf)


# Resultado 3D Systems
# Este é iD do assignee:

assignees_3d = search_pv(query = qry_funs$contains(assignee_organization = "Corp Z"),
          endpoint = "assignees")

assignees_3d$data$assignees


resultados_stratasys = search_pv(query = qry_funs$contains(assignee_organization = c("Stratasys", "Objet Geometries Ltd","MakerBot Industries, LLC"),
                       fields = c("patent_id", "patent_number", "patent_title", "patent_year", "patent_type"), all_pages = TRUE)

View(resultados_stratasys$data$patents)



resultados_3Dsystems = search_pv(query = qry_funs$contains(assignee_organization = c("3d Systems")),
                                 fields = c("patent_id", "patent_number", "patent_title", "patent_year", "patent_type"), all_pages = TRUE)




resultados = search_pv(query = qry_funs$eq(assignee_id = "30f6122b2c1fa7d504fa8306f8be21e1"),
                       fields = c("patent_id", "patent_number", "patent_title", "patent_year", "patent_type"), all_pages = TRUE)




View(resultados$data$patents)



resultados = search_pv(query = qry_funs$contains(assignee_organization = "3D SYSTEMS"),
          endpoint = "assignees")




search_pv(query = qry_funs$contains(inventor_last_name = "smith"),
         endpoint = "assignees")

#...Then pass to search_pv:
search_pv(query =
            '
{"_gte":{"patent_date":"2007-01-04"}}
'
          ,
          fields = fields, endpoint = "patents")





teste_patentes = search_pv(query = qry_3, fields = c("patent_id", "patent_number", "patent_title", "patent_year", "patent_type"), all_pages = TRUE)


patentes = as.data.frame(teste_patentes$data$patents)



patentsview::qry_funs


#### Código Não utilizado ####

#### 4.2.2 Opção 2.0: Passado + Futuro Filtrado 
# Opção 2.0 - Atual: Rodar Passado e Futuro, e tomar providências para que os resultados do passado não sejam levadosem consideração.
# Esta opção é mais alinhada ao procedimento de calibração utilizado na dinâmica de sistemas, porém admite diferentes trajetórias das variáveis, inclusive no passado.
# Por um lado, esta opção é mais conservadora (porque limita o que o futuro pode ser com base no histórico).
ensemble_a_simular = resultados_casos_plausiveis$Ensemble[which(resultados_casos_plausiveis$Ensemble[,opcoes$VarCenarios] %in% cenarios_considerados_plausiveis),]

opcoes$SimularApenasCasoBase = FALSE
opcoes$N = n_ensemble_total
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = TRUE
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_opcao2.0_passado_e_futuro
opcoes$Paralelo = TRUE
START<-2007; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')
# Definir período de simulação das estratégias (e forma de "mudar a estratégia" no primeira ano.)

results2.0 = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                              sdmodel = sdmodel, 
                                              opcoes = opcoes,
                                              ensemble = ensemble_a_simular)

# Salvar resultados:
save(results2.0, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/results2.0.rda")

# load("/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/results2.0.rda")


#### 4.2.2 Opção 2.1: Futuro Filtrado com Condições Iniciais do Caso Base
# Opção 2.1: Rodar Apenas futuro, usando condições iniciais do caso base, e filtrando ensemble com parâmetros plausíveis do passado.
# Esta opção é uma derivacao da opção 2, porém admite que o passado foi igual ao cenário base.
# Esta opção funciona da seguinte maneira: Os estoques (condições iniciais do modelo) são retirados do cenário base definido.
# Em seguida, Todos os parâmetros do modelo podem mudar, no ano inicial de simulação.
opcoes$SimularApenasCasoBase = FALSE
opcoes$N = n_ensemble_total
INICIALIZAR_ESTOQUES_COM_CASO_BASE = TRUE
SIMULAR_HISTORICO_DIFERENTE = FALSE
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_opcao2.0_passado_e_futuro
opcoes$Paralelo = TRUE
START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')
# Definir período de simulação das estratégias (e forma de "mudar a estratégia" no primeira ano.)

results2.1 = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                              sdmodel = sdmodel, 
                                              opcoes = opcoes,
                                              ensemble = ensemble_a_simular)

# Salvar resultados:
save(results2.1, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/results2.1.rda")





# CART
library(rpart)

# grow tree
cart_fit = rpart(y ~., data = x, method = "class")

printcp(cart_fit) # display the results
plotcp(cart_fit) # visualize cross-validation results
summary(cart_fit) # detailed summary of splits

# Observando a Importância das Variáveis
cart_fit$variable.importance

View(cart_fit$variable.importance)

summary(cart_fit)

plot(cart_fit$variable.importance)


caret::varImp(cart_fit)

rpart.plot::prp(fit, digits = 3, clip.right.labs = FALSE, varlen = 0)

### Rodando o CART com o caret
library(caret)
cartFit <- train(y ~ ., #i.e., "Class is defined as all variables in the data frame
                 data = x, #defining the dataframe as Sonar, from mlbench
                 method="rpart",
                 trControl = trainControl(method = "boot",
                                          returnResamp="all"))
varImp(cartFit)



# Classification Tree with rpart

results.prim = prim.box(x = x, y = y, threshold.type = 1, peel.alpha = 0.25, paste.alpha = 0.15, threshold = 0.3)

summary(results.prim, print.box = TRUE)

write.csv(ensemble_e_resultados)

arquivo_ensemble_e_resultados = "ensemble_e_resultados.rda"

arquivo_salvar = ensemble_e_resultados[,c(variavel_resposta,variaveis_incertas)]

save(arquivo_salvar, file = arquivo_ensemble_e_resultados)

library(sdtoolkit)


sdtoolkit::sdprim(x = x, y = y)

sdtoolkit::sd.start()


# Descoberta de Cenários.

# Descoberta de Cenários - Com ajuda do OpenMORDM
assign("mordm.globals", new.env())
factors = x 
response = y


box = prim.box(x = x, y = y, threshold.type = 1, peel.alpha = 0.25, paste.alpha = 0.15, threshold = 0.3)

bounds = NULL
which.box=1

box <- prim.box(x = factors, y = response, threshold.type = 1, peel.alpha = 0.25, paste.alpha = 0.15, threshold = 0.3)

marks <- lapply(1:box$num.hdr.class, function(i) {
  i <- eval(i)
  colnames(box$box[[i]]) <- colnames(factors)
  mordm.mark.box(box$box[[i]], box$y.fun[i], box$mass[i])
})

if (is.null(bounds)) {
  bounds <- apply(factors, 2, range)
}

dummy.data <- list()
attr(dummy.data, "nvars") <- ncol(factors)
attr(dummy.data, "bounds") <- bounds
mordm.plot.box(dummy.data, marks[[which.box]])

varargs$threshold.type = -1
# compute density and coverage of the box
varargs <- list(...)

if (is.null(varargs$threshold.type) || varargs$threshold.type==0) {
  
} else if (varargs$threshold.type == -1) {
  threshold <- mean(response)
  total.interesting = sum(response <= threshold)
  
  captured.indices <- mordm.select.indices(factors, mordm.mark.union(marks))
  captured.interesting = sum(response[captured.indices] <= threshold)
  
  cat("Coverage: ")
  cat(captured.interesting / total.interesting)
  cat("\n")
  cat("Density: ")
  cat(captured.interesting / length(captured.indices))
  cat("\n")
} else {
  threshold <- mean(response)
  total.interesting = sum(response >= threshold)
  
  captured.indices <- mordm.select.indices(factors, mordm.mark.union(marks))
  captured.interesting = sum(response[captured.indices] >= threshold)
  
  cat("Coverage: ")
  cat(captured.interesting / total.interesting)
  cat("\n")
  cat("Density: ")
  cat(captured.interesting / length(captured.indices))
  cat("\n")
}

invisible(marks)




# Descrição dos Cenários Desafiadores para a Estratégia.






# Rodando um Cenário Base
## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
START<-0; FINISH<-10; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; BROWSE_ON_DIFF = TRUE
VERIFICAR_GLOBAL = FALSE;

## Carregando Modelo
source('modelo-calibracao.R', encoding = 'UTF-8')

# Carregando Variáveis de Output do Ithink para Comparação
arquivo_excel_stocks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_stocks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"))
arquivo_excel_checks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_checks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"))
arquivo_excel_global = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_tudo.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"))


dados_ithink_stocks  = arquivo_excel_stocks$ResultadosIthink %>% dplyr::select(-Months)
dados_ithink_checks  = arquivo_excel_checks$ResultadosIthink %>% dplyr::select(-Months)

dados_ithink_global = arquivo_excel_global$ResultadosIthink %>% dplyr::select(-Months)


variaveis_ithink_stocks = names(dados_ithink_stocks)
variaveis_ithink_checks = names(dados_ithink_checks)


variaveis_globais_a_verificar = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/variaveis_globais_a_verificar.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"))
variaveis_globais_a_verificar = as.vector(variaveis_globais_a_verificar$ResultadosIthink$variaveis)



# Rodando a Simulação com os Parâmetros do Sterman, rodando uma vez apenas.
arquivo_parametros = "./calibracao/params_calibracao.xlsx"

parametros_completos = readxl::read_xlsx(arquivo_parametros, sheet = "params")

parametros_cenariobase = t(parametros_completos[,"CenarioBase"])[1,]

names(parametros_cenariobase) = as.matrix(parametros_completos[,1])

# Mudando o tempo de simulação para Simular o Sterman
resultados_cenariobase = solve_modelo_dissertacao(parametros = parametros_cenariobase, modelo = sdmodel$Modelo, simtime = sdmodel$SimTime)






















# Visualizando que o Cenário Base é Plausível
START<-2007; FINISH<-2037; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.01; BROWSE_ON_DIFF = TRUE
VERIFICAR_GLOBAL = FALSE;

resultados_cenarioscalibracao = simularRDM_e_escolher_estrategia(inputs = planilha_inputs, sdmodel = sdmodel, opcoes = opcoes)

resultados_cenarioscalibracao$Ensemble = adicionar_erro_ao_ensemble(results = resultados_cenarioscalibracao, variavel_calibracao = "fIndustryOrderRate", planilha_calibracao = "./calibracao/dados_calibracao.xlsx", opcoes = opcoes)

dados_calibracao <- as.data.frame(read_xlsx(path = "./calibracao/dados_calibracao.xlsx", sheet = "Plan1"))

hist(resultados_cenarioscalibracao$Ensemble[,"SumOfSquareResiduals"])

quartil1_erro = quantile(resultados_cenarioscalibracao$Ensemble[,"SumOfSquareResiduals"], probs = c(0.25))

cenarios_quartis = resultados_cenarioscalibracao$Ensemble[which(resultados_cenarioscalibracao$Ensemble[,"SumOfSquareResiduals"]<quartil1_erro),opcoes$VarCenarios]

cenario_menor_erro = resultados_cenarioscalibracao$Ensemble[which(resultados_cenarioscalibracao$Ensemble[,"SumOfSquareResiduals"]==min(resultados_cenarioscalibracao$Ensemble[,"SumOfSquareResiduals"])),opcoes$VarCenarios]

# Selecionando Pontos de Dados para Exibir no Gráfico
time_points<-seq(from=1, to=length(SIM_TIME),by=1/STEP)

time_plot = seq(from=START, to=FINISH)

resultados_exibir = dplyr::filter(resultados_cenarioscalibracao$DadosSimulados, Scenario == cenario_menor_erro)[time_points,]



p1<-ggplot()+
  geom_point(data=dados_calibracao,size=1.5,aes(time,fIndustryOrderRate,colour="Data"))+
  geom_line(data=resultados_exibir,size=1,aes(x=time,y=fIndustryOrderRate,colour="Model"))+
  ylab("Demanda Anual - Impressoras 3D > 5k USD")+
  xlab("Anos")+
  scale_y_continuous(labels = comma)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Dados",
                               "Modelo"))
p1


resultados_exibir_outros_parametros = resultados_cenarioscalibracao$DadosSimulados[which(results$DadosSimulados[,opcoes$VarCenarios] %in% cenarios_quartis),]


p2<-ggplot()+
  geom_point(data=dados_calibracao,size=1.5,aes(time,fIndustryOrderRate,colour="Data"))+
  geom_line(data=resultados_exibir_outros_parametros,size=0.5,aes(x=time,y=fIndustryOrderRate,colour="Model", group = Scenario, color = factor(Scenario)))+
  ylab("Demanda da Indústria")+
  xlab("Anos")+
  scale_y_continuous(labels = comma)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Dados",
                               "Modelo"))
p2














plot_linha_uma_variavel_ensemble(dados = resultados_cenarioscalibracao, variavel = "fIndustryOrderRate", nome_amigavel_variavel = "Demanda Global Impressoras Profissionais", estrategia = 1)

View(resultados_exibir_outros_parametros)








# RODADA 1

## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
START<-0; FINISH<-10; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.00001; BROWSE_ON_DIFF = FALSE

# Carregando o Modelo Novamente:
source('modelo-calibracao.R', encoding = 'UTF-8')

results = simularRDM_e_escolher_estrategia(inputs = "./rodada1/params_rodada1.xlsx", sdmodel = sdmodel, opcoes = opcoes)

save(results, file = "./rodada1/resultados.Rdata")

dplyr::group_by(results$DadosSimulados, Scenario) %>% dplyr::summarise(MaxTime = max(time))


results$DadosUltimoPeriodo

# Salvando Resultados

# Gráficos

plots_rodada1 = list(
  plot_estrategia1 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = 1),
  plot_1_e_candidata = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = c(1, results$EstrategiaCandidata)),
  plot_preco_estrategia10 = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sPrice1", nome_amigavel_variavel = "Preço", estrategia = c(3)),
  plot_estrategia_candidata = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "VPL", estrategia = results$EstrategiaCandidata),
  plot_whisker_lever_perc_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1RegretPerc"),
  plot_whisker_lever_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1Regret"),
  plot_whisker_lever_profit = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1")
)



# Verificar resultados com NA

# Salvar Plots em Imagem:
mapply(ggsave, file=paste0("./images/", names(plots_rodada1), ".png"), plot=plots_rodada1, width = plots_width, height = plots_heigh)


### Analisar ENsemble

ensemble_analisado = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble,
                                                             dados_regret = results$AnaliseRegret$Dados, 
                                                             var_cenarios = opcoes$VarCenarios, 
                                                             var_estrategias = opcoes$VarEstrategias, 
                                                             var_resposta = opcoes$VarResposta, 
                                                             estrategia_candidata = results$EstrategiaCandidata)


parametros_lidos = readxl::read_xlsx("./rodada1/params.xlsx", sheet = "params")

incertezas = parametros_lidos[which(!(parametros_lidos[,"Min"]==parametros_lidos[,"Max"])),"Variavel"][[1]]

incertezas = c("aLCStrength", "aNormalCapacityUtilization", "aSensOfAttractToPrice", "aRatioOfFixedToVarCost")


# Ajuda a Identificar em Que condições a Estratégia Candidata não é a melhor.
plot_estrategias_incertezas = plot_estrategias_versus_incertezas(ensemble_analisado = ensemble_analisado,incertezas = incertezas)

ggsave(filename = "./images/plot_estrategias_incertezas.png", plot = plot_estrategias_incertezas, width = 10, height = 6)




### Rodadno Feature Selection para Saber as Incertezas mais relevantes (computacionalmente):
# https://www.linkedin.com/pulse/r-finding-most-important-predictor-variables-saranya-anandh/
# Gerando um ensemble ampliado para a estratégia 4:
estrategia = results$EstrategiaCandidata
ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")

ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia),]

variaveis_incertas = colnames(results$Ensemble)

variaveis_incertas = variaveis_incertas[which(variaveis_incertas %in% names(ensemble_e_resultados))]

variavel_resposta = "sNPV1ProfitRegret"

# Os dados do ultimo período não foram substituidos (por algum motivo que eu não sei qual é.)
class(ensemble_e_resultados)

ensemble_e_resultados$sNPVProfit1Regret


library(party)
cf1 <- cforest(ensemble_e_resultados[,variavel_resposta] ~ . , data= ensemble_e_resultados[,variaveis_incertas], control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
varimp(cf1) # get variable importance, based on mean decrease in accuracy#=>                 Month          Day_of_month           Day_of_week #=>           0.689167598           0.115937291          -0.004641633 #=>       pressure_height            Wind_speed              Humidity #=>           5.519633507           0.125868789           3.474611356 #=>  Temperature_Sandburg   Temperature_ElMonte Inversion_base_height #=>          12.878794481          14.175901506           4.276103121 #=>     Pressure_gradient Inversion_temperature            Visibility #=>           3.234732558          11.738969777           2.283430842
varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors#=>                 Month          Day_of_month           Day_of_week #=>            0.08899435            0.19311805            0.02526252 #=>       pressure_height            Wind_speed              Humidity #=>            0.35458493           -0.19089686            0.14617239 #=>  Temperature_Sandburg   Temperature_ElMonte Inversion_base_height #=>            0.74640367            1.19786882            0.69662788 #=>     Pressure_gradient Inversion_temperature            Visibility #=>            0.58295887            0.65507322            0.05380003
varimpAUC(cf1)  # more robust towards class imbalance.#=>                 Month          Day_of_month           Day_of_week #=>            1.12821259           -0.04079495            0.07800158 #=>       pressure_height            Wind_speed              Humidity #=>            5.85160593            0.11250973            3.32289714 #=>  Temperature_Sandburg   Temperature_ElMonte Inversion_base_height #=>           11.97425093           13.66085973            3.70572939 #=>     Pressure_gradient Inversion_temperature            Visibility #=>            3.05169171           11.48762432            2.04145930


library(relaimpo)
lmMod <- lm(ensemble_e_resultados$sNPVProfit1Regret ~ . , data = ensemble_e_resultados[,variaveis_incertas])  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)
calc.relimp(lmMod, rela = TRUE)
# calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # rel

# por algum motivo o discount Rate não está lá dentro:

library(breakDown)
br = broken(lmMod, new_observation = ensemble_analisado[18,])
plot(br)


### O resultado desta análise fez um pouco de sentido (e não é óbvio).
library(earth)
marsModel <- earth(ensemble_e_resultados$sNPVProfit1Regret ~ ., data=ensemble_e_resultados[,variaveis_incertas]) # build model
ev <- evimp(marsModel) # estimate variable importance#=>                       nsubsets   gcv    rss#=> Temperature_ElMonte         29 100.0  100.0#=> Pressure_gradient           28  42.5   48.4#=> pressure_height             26  30.1   38.1#=> Month9                      25  26.1   34.8#=> Month5                      24  21.9   31.7#=> Month4                      23  19.9   30.0#=> Month3                      22  17.6   28.3#=> Inversion_base_height       21  14.4   26.1#=> Month11                     19  12.3   24.1#=> Visibility                  18  11.4   23.2#=> Day_of_month23              14   8.9   19.8#=> Humidity                    13   7.4   18.7#=> Month6                      11   5.1   16.6#=> Temperature_Sandburg         9   7.0   15.6#=> Wind_speed                   7   5.1   13.4#=> Month12                      6   4.2   12.3#=> Day_of_month9                3   4.6    9.1#=> Day_of_week4                 2  -3.9    5.9#=> Day_of_month7-unused         1  -4.7    2.8
plot(ev)


# Stepwise (Também faz sentido)
base.mod <- lm(ensemble_e_resultados$sNPVProfit1Regret ~ 1 , data= ensemble_e_resultados[,variaveis_incertas])  # base intercept only model
all.mod <- lm(ensemble_e_resultados$sNPVProfit1Regret ~ . , data= ensemble_e_resultados[,variaveis_incertas]) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)

# Rodando o Algoritmo Boruta
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(ensemble_e_resultados$sNPVProfit1Regret ~ ., data=na.omit(ensemble_e_resultados[,variaveis_incertas]), doTrace=2)  # perform Boruta search# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables#=> [1] "Month"                 "ozone_reading"         "pressure_height"      #=> [4] "Humidity"              "Temperature_Sandburg"  "Temperature_ElMonte"  #=> [7] "Inversion_base_height" "Pressure_gradient"     "Inversion_temperature"#=> [10] "Visibility"
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance



landscape_estrategia1 = plot_landscape_futuros_plausiveis(
  results, estrategia = 2, 
  variavelresp = "sNPVProfit1Regret",
  nomeamigavel_variavelresp = "VPL",
  variavel1  = "aRatioOfFixedToVarCost",
  n_variavel1 = "Sensibilidade ao Preço",
  variavel2 = "aNormalCapacityUtilization",
  n_variavel2 = "Utilização da Capacidade"
)
























# RODADA 2

## Gerar Capitulo 4:
rmarkdown::render(input = "Capitulo-4.Rmd")


# Simulando uma vez apenas
resultado_unico <- data.frame(ode(y=stocks, times=simtime, func = modelo, 
                  parms=auxs, method="euler"))




## GE


resultado_transposto = t(resultado_unico)

write.csv2(x = resultado_transposto, file = "resultadosdoR.csv")


results = simularRDM_e_escolher_estrategia(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes)

incertezas = c("aAdvertisingEffectiveness", "aContactRate", "aAdoptionFraction", "aAdvertisingCost", "aAverageTicket")

ensemble_analisado = analisar_ensemble_com_melhor_estrategia(ensemble = results$Ensemble, dados_regret = results$AnaliseRegret$Dados, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias, var_resposta = opcoes$VarResposta, estrategia_candidata = results$EstrategiaCandidata)

plot_estrategias_versus_incertezas(ensemble_analisado, incertezas)










lever = results$EstrategiaCandidata
variaveis = c("CashRegretPerc","CashRegret","Cash", "AverageTicket", "AdvertisingCost", "AdvEffectiveness", "ContactRate", "AdoptionFraction")

sdrdm.pairs_plot(data = results$AnaliseRegret$Dados, lever = 1, variables = variaveis)


grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "Cash")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "Adopters")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "CashRegretPerc")

grafico_whisker_por_lever(dados_regret = results$AnaliseRegret$Dados, variavel = "CashRegret")





# Considerando o último ano

# + guides(color=FALSE) +

# + scale_color_manual(values = Mypalette(5))

# # Fazer Depois: Considerar Estratégias
# # expand.grid(ensemble, as.matrix(inputs$Levers))
# 
# # A biblioteca plotly entra em conflito com uma função usada pelo OpenMORDM!
# library(OpenMORDM)
assign("mordm.globals", new.env())
# # Tentando fazer scenario discovery
# # assign("mordm.globals", new.env(), envir=parent.env(environment()))
# 
# 
# dados_sd = dplyr::filter(dados_simulacao, Tempo == FINISH) 
# 
# # Variáveis a considerar na análise
# vfatores = c("AdvEffectiveness", "ContactRate", "AdoptionFraction")
# factors = dados_sd[,vfatores]
# 
# vresposta = c("Adopters")
# response = dados_sd[,vresposta]
# # response = (dados_sd[,2] > 15000) * 1
# thr = 987470
# thr.type = -1 # >= x
# 
# # Esta linha de código agora funciona:
# analise_prim = analyze.prim(factors, response, threshold = thr, threshold.type = thr.type, which.box = 1)
# 
# # Aqui abaixo há
# box_prim = prim::prim.box(x = factors, y = response, threshold = thr, threshold.type = thr.type)
# 
# # Até aqui não consegui 
# 
# 
# # Analisar com o PRIM / MORDM.
# library(prim)
# dados_prim = dados_simulacao # dplyr::filter(dados_simulacao, Tempo == FINISH)
# factors = as.matrix(dados_prim[,c(4:11,1)])
# response = as.matrix(dados_prim[3])
# 
# analyze.prim(factors, response, threshold.type=-1,
#              threshold=10661)
# 
# mordm.correlation(factors, ht = 0.75, lt = 0.25, all = FALSE)
# 
# box = prim.box(x = factors, y = response, threshold = 10661, threshold.type = -1)
# 
# 
# 
# 
# # # Visualizando os Resultados
# 
# dados = dados_simulacao
# x_axis_name = "Tempo"
# y_axis_name = "Adopters"
# 
# 
# 
# 
# 
# # Não funcionou o gráfico por Leveler (por algum problema ainda não identificado, e não são os Nas e Infinitos.)
# 
# 
# 
# 
# 
# 
# 
# # Visualizando Todas as Replicações
# ggplot(dados_simulacao,
#        aes(x=Tempo, y=Adopters, color=Replicacao, group=Replicacao)) + 
#   geom_line() + 
#   ylab("Adopters") + 
#   xlab("Tempo") + guides(color=FALSE)
# 
# 
# # 




# Armazenando os Resultados
# write.csv(dados_simulacao, file = "dados_simulados_difusao.csv", row.names = FALSE)


# Replicando Sterman
## Inicializar variaveis da simulação aqui (antes de carregar o modelo.)
START<-0; FINISH<-40; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.00001; BROWSE_ON_DIFF = FALSE

## Carregando Modelo
source('modelo-calibracao.R', encoding = 'UTF-8')

# Rodando a Simulação com os Parâmetros do Sterman, rodando uma vez apenas.
arquivo_parametros = "./analise-sterman/params_sterman.xlsx"

parametros_completos = readxl::read_xlsx(arquivo_parametros, sheet = "params")

parametros_sterman = t(parametros_completos[,"Sterman"])[1,]

names(parametros_sterman) = as.matrix(parametros_completos[,1])

# Mudando o tempo de simulação para Simular o Sterman
resultados_sterman = solve_modelo_dissertacao(parametros = parametros_sterman, modelo = sdmodel$Modelo, simtime = sdmodel$SimTime)

# Gerando Gráficos
sterman_plots = list(
  grafico_npv_sterman = plot_linha_uma_variavel(dados = resultados_sterman, variavel = "sNPVProfit1", nome_amigavel_variavel = "Valor Presente Liquido"),
  
  grafico_preco_sterman = plot_linha_uma_variavel(dados = resultados_sterman, variavel = "sPrice1", nome_amigavel_variavel = "Preço Player 1"),
  
  grafico_demanda_sterman = plot_linha_uma_variavel(dados = resultados_sterman, variavel = "fIndustryOrderRate", nome_amigavel_variavel = "Demanda Anual Total"),
  
  grafico_vpl_preco = plot_linha_duas_variaveis(dados = resultados_sterman, variavel1 = "sNPVProfit1", variavel2 = "sPrice1", nome_amigavel_variavel1 = "VPL", nome_amigavel_variavel2 = "Preço"),
  
  grafico_vpl_demanda = plot_linha_duas_variaveis(dados = resultados_sterman, variavel1 = "sNPVProfit1", variavel2 = "fIndustryOrderRate", nome_amigavel_variavel1 = "VPL", nome_amigavel_variavel2 = "Demanda Global")
  
)

# Salvando Resultados do Sterman e Plots (para eventual verificação posterior):
save(sdmodel, parametros_sterman, resultados_sterman, sterman_plots, file = "./analise-sterman/resultados_sterman.Rdata")
# Para carregar depois, basta rodar:
# load(file = "./analise-sterman/resultados_sterman.Rdata")

# Salvando todos os Gráficos do Sterman:
mapply(ggsave, file=paste0("./images/", names(sterman_plots), ".png"), plot=sterman_plots, width = plots_width, height = plots_heigh)



#### Gerar Relatório ####
rmarkdown::render(input = "Resultados.Rmd")
