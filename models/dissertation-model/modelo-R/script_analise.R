# Esse script Roda apena o necessário para gerar as simulações e salva-las em um arquivo independente.

# Carregando Funções Úteis
START<-2007; FINISH <-2017; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')


USAR_DADOS_SALVOS = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE



#### 4.0 Configurando Simulações ####

# Gerar casos para simulação com base em estimativa inicial de parâmetros.
opcoes_iniciais = list(
  VarResposta = "sNPVProfit1",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 30,
  VarTempo = "time",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min",
  Paralelo = TRUE,
  ModoParalelo = "FORK",
  SimularApenasCasoBase = TRUE,
  FullFactorialDesign = TRUE,
  FiltrarCasosPlausiveis = TRUE
)

opcoes = opcoes_iniciais


# Planilhas de Configuração das Simulação:
planilha_simulacao_calibracao_historico = "./calibracao/params_calibracao_com_estrategia_v2.xlsx"

planilha_simulacao_opcao1_futuro = "./calibracao/params_calibracao_opcao1.xlsx"

planilha_opcao2.0_passado_e_futuro = planilha_simulacao_calibracao_historico

planilha_opcao2.1_futuro = planilha_simulacao_calibracao_historico

percentil_utilizado_como_criterio = c(PercentilCriterio = 0.5)

# Número de casos TOTAL a rodar (considerando todas as estratégias e todos os cenários).
n_casos_total = 10800 # 400
n_estrategias = nrow(carregar_inputs(arquivo_de_inputs = planilha_simulacao_calibracao_historico, opcoes = opcoes)$Levers)

# Tamanho do Ensemble Adimitido (para simular todas as estratégias)
n_ensemble_total = round(n_casos_total / n_estrategias, 0) 

# Tamanho do ensemble para calibração.
n_ensemble_calibracao = round(n_ensemble_total / percentil_utilizado_como_criterio,0)

#### 4.1 Calibração com Dados Históricos de Demanda ####
# Simulação 0: Simulando Histórico e Observando Fit do Modelo:
###
opcoes$SimularApenasCasoBase = TRUE
opcoes$N = n_ensemble_calibracao
# Esta opção faz com que os estoques sejam inicializados com o valor inicial dos estoques no cenário base.
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
# Esta opção é utilizada para modificar o comportamento de variáveis selecionadas (ex.: Estratégia do player) durante o período histórico.
# Se ativado, o período de tempo anterior ao ANO_INICIO_AVALIACAO assume variáveis com um valor "default", e o NPV dos players não é
# modificado enquanto até o ANO_INICIO_AVALIACAO. Se não ativado, a simulação ocorre normalmente.
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_calibracao_historico
opcoes$Paralelo = TRUE
opcoes$FiltrarCasosPlausiveis = FALSE
# Rodar Simulação:
START<-2007; FINISH <-2017; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')
resultados_casos_plausiveis = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                                               sdmodel = sdmodel,
                                                               opcoes = opcoes)

# Salvar resultados com casos plausíveis:
save(resultados_casos_plausiveis, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/resultados_casos_plausiveis.rda")



variavel_calibracao = "fIndustryOrderRate"
nome_amigavel_variavel_calibracao = "Demanda Imp. 3D > 5000 USD"

# Parâmetros Utilizados
resultados_casos_plausiveis$Inputs$Parametros

# Mostrar Ensemble.

head(resultados_casos_plausiveis$Ensemble, 10)
# Gerar 10.000 replicações e salvar.

# Mostrar Dados Simulados
head(resultados_casos_plausiveis$DadosSimulados, 20)

# Mostrar Variável de Demanda em Todos os Cenários (Sem Filtro)

cenarios_a_exibir_grafico = sample(1:opcoes$N,size = min(30,opcoes$N))


plot_demanda_pre_calibracao = plot_linha_uma_variavel_ensemble(dados = subset(resultados_casos_plausiveis$DadosSimulados, Scenario %in% cenarios_a_exibir_grafico), 
                                                               variavel = variavel_calibracao, 
                                                               nome_amigavel_variavel = nome_amigavel_variavel_calibracao) + geom_vline(xintercept = 2017)

plot_demanda_pre_calibracao = plot_demanda_pre_calibracao + annotate("text", x = 2017.2, y = max(resultados_casos_plausiveis$DadosSimulados$fIndustryOrderRate), label=c("Hoje"),hjust=0)

plot_demanda_pre_calibracao

# Comparar Simulações com Dados históricos de Demanda
ensemble_com_erro = adicionar_erro_ao_ensemble(results = resultados_casos_plausiveis, variavel_calibracao = variavel_calibracao, planilha_calibracao = "./calibracao/dados_calibracao.xlsx", opcoes = opcoes)
ensemble_com_erro = as.data.frame(ensemble_com_erro)


# Exibir Comparação com Dados Históricos.

variaveis_analise_fit = c("SumOfSquareResiduals", "MeanSquareError", "MeanAbsoluteError", "MeanAbsolutePercentError", "UM_ThielBiasDiffMeans", "US_ThielUnequalVariation", "UC_ThielUnequalCovariation")
variaveis_exibir_ensemble = c("Scenario", variaveis_analise_fit)
cenarios_a_exibir_tabela = sample(1:opcoes$N,size = 5)

t(ensemble_com_erro[1:5,variaveis_exibir_ensemble])

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
  geom_point(data=dados_calibracao,size=1.5,aes(time,fIndustryOrderRate,colour="Data"))+
  geom_line(data=resultados_exibir,size=1,aes(x=time,y=fIndustryOrderRate,colour="Model"))+
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

# Definir um threshold de aceitabilidade do Erro quadrado médio.

# Definir um threshold mínimo da demanda e máximo da demanda (anual).

# Definir casos considerados plausíveis.




percentil_ssr = quantile(ensemble_com_erro[,"SumOfSquareResiduals"], probs = c(percentil_utilizado_como_criterio))

cenarios_quartis = ensemble_com_erro[which(ensemble_com_erro[,"SumOfSquareResiduals"] <= percentil_ssr),opcoes$VarCenarios]

cenarios_considerados_plausiveis = cenarios_quartis

# Como critério irei utilizar apenas os casos que erram em média menos do que 30 %.
#erro_percentual_medio_maximo = 0.5

dados_calibracao$Scenario = 1000

# cenarios_considerados_plausiveis = ensemble_com_erro[which(ensemble_com_erro$MeanAbsolutePercentError < erro_percentual_medio_maximo),opcoes$VarCenarios]

plot_cenarios_plausiveis = plot_linha_uma_variavel_ensemble(dados = resultados_casos_plausiveis$DadosSimulados[which(resultados_casos_plausiveis$DadosSimulados$Scenario %in% cenarios_considerados_plausiveis),]
                                                            ,variavel = variavel_calibracao, nome_amigavel_variavel = nome_amigavel_variavel_calibracao) + geom_point(data=dados_calibracao, size = 1.5, aes(time, fIndustryOrderRate))

plot_cenarios_plausiveis

#### 4.2 Simulação dos Casos Contra Estratégias ####

# CALIBRAÇÃO COM DADOS HISTÓRICOS
# Simulação 0: Simulando Histórico e Observando Fit do Modelo:
###


#### 4.2.2 Opção 1: Apenas Futuro ####
# Opção 1: Dados para Simular o Futuro, sem comparação com o Passado. Usar filtro para a demanda máxima e mínima a posteriori.
# Esta opção é inspirada na abordagem utilizada por Lempert.
opcoes$FiltrarCasosPlausiveis = TRUE
opcoes$SimularApenasCasoBase = FALSE
opcoes$N = n_ensemble_total
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_opcao1_futuro
opcoes$Paralelo = TRUE
START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')

# Simular
results1 = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                            sdmodel = sdmodel, 
                                            opcoes = opcoes)

# Salvar resultados:
save(results1, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/results1.rda")


# Aqui ainda seria necessário filtar os resultados com o critério definido.

#### 4.2.2 Opção 2.0: Passado + Futuro Filtrado ####
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


#### 4.2.2 Opção 2.1: Futuro Filtrado com Condições Iniciais do Caso Base####
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
save(results2.1, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/results2.1_bc.rda")

