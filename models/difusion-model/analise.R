# Este exemplo é baseado no modelo de difusão do capítulo 6 do livro do Morecroft.

# Loading Libraries
# library(ggplot2)
# require(gridExtra)
# library(scales)
# library(dplyr)
# # library(plotly)
library(akima)
# library(prim)

# library(OpenMORDM)

## Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

## Carregando o Modelo
source('modelo-difusao.R', encoding = 'UTF-8')

# source('OpenMORDM.R')

# Carregando Inputs
inputs = carregar_inputs()

# Obter Ensemble LHS (Sem Variáveis das Estratégias)
ensemble = obter_lhs_ensemble(params = inputs$Parametros, n = 100)

# Ampliar Ensemble com as variáveis das Estratégias
novo_ensemble = ampliar_ensemble_com_levers(ensemble = ensemble, levers = inputs$Levers)

# Rodando a Simulação
dados_simulacao = simular(stocks = stocks, simtime = simtime, modelo = modelo, ensemble = novo_ensemble, nomes_variaveis_final = nomes_variaveis_final)


library(ggplot2)
# Visualizando Todas as Replicações
ggplot2::ggplot(dados_simulacao,
       aes(x=Tempo, y=Adopters, color=factor(Lever), group=Replicacao)) + 
  geom_line() + 
  ylab("Adopters") + 
  xlab("Tempo") +
  labs(color = "Estratégia")
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

dadosplot = subset.data.frame(dados_simulacao, (Tempo == FINISH & Lever == 5)) %>% select(Adoption_Rate, ContactRate, Adopters)

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
plot_ly(x = s$Adoption_Rate, y = s$ContactRate, z = s$Adopters) %>% add_surface() %>% layout(xaxis = x, yaxis = y)


# Armazenando os Resultados
# write.csv(dados_simulacao, file = "dados_simulados_difusao.csv", row.names = FALSE)
