###############################################################
# Autor: Pedro Nascimento de Lima, 2017
# Código fonte desenvolvido para a Dissertação de Mestrado.
# Arquivo: analise.R
# Objetivo: Este arquivo contém funções utilizadas para as análises 
# RDM realizadas durante a dissertação.
###############################################################

library(akima)
# library(prim)


# Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

## Carregando o Modelo, e outros objetos
source('modelo-difusao.R', encoding = 'UTF-8')

opcoes = list(
  VarResposta = "Cash",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 100,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

results = simularRDM_e_escolher_estrategia(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes)


write.csv2(results$DadosSimulados, "./resultados/dadossimulados.csv")

write.csv2(results$DadosUltimoPeriodo, "./resultados/dadosultimoperiodo.csv")

write.csv2(results$AnaliseRegret$ResumoEstrategias, "./resultados/resumoestrategias.csv")


# Gráfico 1: Valor em Caixa da Estratégia 1 em todos os Cenários
gr1_dados = subset(results$DadosSimulados, Lever == 1)
ggplot2::ggplot(gr1_dados,
                aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Valor Presente") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


# Gráfico 2: Número de Clientes em todos os cenários, na estratégia 1 - sem propaganda.
gr2_dados = subset(results$DadosSimulados, (Lever == 1))
ggplot2::ggplot(gr2_dados,
                aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Clientes") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


## Observando o número de clientes na estratégia 10
gr4_dados = subset(results$DadosSimulados, (Lever == 10))
ggplot2::ggplot(gr4_dados,
                aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Clientes") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


# Observando o número de clientes na estratégia 20
gr3_dados = subset(results$DadosSimulados, (Lever == 20))
ggplot2::ggplot(gr3_dados,
                aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Clientes") + 
  xlab("Tempo") +
  labs(color = "Estratégia")



# Observando o Comportamento de um cenário apenas em todas as 20 estratégias:

scenario1 = 20
levers = c(10,15,20)
gr4_dados = dplyr::filter(results$DadosSimulados, Scenario == 20, Lever == 20)
ggplot2::ggplot(gr4_dados,
                aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Clientes") + 
  xlab("Tempo") +
  labs(color = "Estratégia")



# Continuar a partir daqui: definir critério de aceitação e seguir com a análise para a identificaçao de cenários.
threshold_regr_percentual = 0.2 # 

dados_por_estrategia = dplyr::group_by(results$AnaliseRegret$Dados, Lever) %>% select(Lever, Scenario, Cash, CashRegret, CashRegretPerc)

dados_por_estrategia$Lever = as.factor(dados_por_estrategia$Lever)

# Gerando Grafico da Variável de Perda de Oportunidade
library(ggplot2)
p <- ggplot(dados_por_estrategia, aes(y = CashRegretPerc,x = Lever, group = Lever))
p + geom_boxplot()
p + geom_boxplot() + geom_jitter(width = 0.2)
p + geom_violin() + geom_jitter(height = 0, width = 0.1)
p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


# Gerando Grafico da Variável de Perda de Oportunidade Percentual
library(ggplot2)
p <- ggplot(dados_por_estrategia, aes(y = Cash_Percent_Regret,x = Lever, group = Lever))
p + geom_boxplot() + geom_jitter(width = 0.2)
p + geom_violin() + geom_jitter(height = 0, width = 0.1)
p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Gerando Grafico da Variável de Resposta                                      
library(ggplot2)
p <- ggplot(dados_por_estrategia, aes(y = Cash,x = Lever, group = Lever))
p + geom_boxplot()



library(ggplot2)
# Visualizando Todas as Replicações
ggplot2::ggplot(dados_simulacao,
       aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
  geom_line() + 
  ylab("Clientes") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


# Plotando o Adoption Rate 
ggplot2::ggplot(dados_simulacao,
                aes(x=Tempo, y=Adoption_Rate, color=factor(Lever), group=Replicacao)) + 
  geom_line() + 
  ylab("Taxa de Adoção") + 
  xlab("Tempo") +
  labs(color = "Estratégia")


# Considerando o último ano

# + guides(color=FALSE) +

# + scale_color_manual(values = Mypalette(5))

# # Fazer Depois: Considerar Estratégias
# # expand.grid(ensemble, as.matrix(inputs$Levers))
# 
# # A biblioteca plotly entra em conflito com uma função usada pelo OpenMORDM!
# library(OpenMORDM)
# assign("mordm.globals", new.env())
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

library(dplyr)
# Exibição - Filtrando as Variáveis a Observar no Gráfico

# De uma hora para outra o filter não está mais funcionando.
# dadosplot = dplyr::filter(dados_simulacao, Tempo == FINISH, Lever == 1) %>% select (Adoption_Rate, ContactRate, Adopters)

dadosplot = subset.data.frame(dados_simulacao, (Tempo == FINISH & Lever == 2)) %>% select(AdoptionFraction, ContactRate, Adopters)

dadosplot = as.matrix(dadosplot)

names = colnames(dadosplot)

s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])

names(s) = names

# Plotando a População Final
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Taxa Nascimento",
  titlefont = f
)
y <- list(
  title = "TaxaMorte",
  titlefont = f
)
z <- list(
  title = "Populacao",
  titlefont = f
)

library(plotly)
plot_ly(x = s$AdoptionFraction, y = s$ContactRate, z = s$Adopters) %>% add_surface() %>% layout(xaxis = x, yaxis = y)


# Armazenando os Resultados
# write.csv(dados_simulacao, file = "dados_simulados_difusao.csv", row.names = FALSE)
