# Este exemplo é baseado no modelo de difusão do capítulo 6 do livro do Morecroft.

# Loading Libraries
library(ggplot2)
require(gridExtra)
library(scales)
library(dplyr)
library(plotly)
library(akima)

## Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

## Carregando o Modelo
source('modelo-difusao.R', encoding = 'UTF-8')

source('OpenMORDM.R')

# Carregando Inputs
inputs = carregar_inputs()

# Obter Ensemble LHS
ensemble = obter_lhs_ensemble(params = inputs$Parametros, n = 2)

# Rodando a Simulação
dados_simulacao = simular(stocks = stocks, simtime = simtime, modelo = modelo, ensemble = ensemble, nomes_variaveis_final = nomes_variaveis_final)

# Fazer Depois: Considerar Estratégias
expand.grid(ensemble, as.matrix(inputs$Levers))


# Analisar com o PRIM:
library(prim)
dados_prim = dplyr::filter(dados_simulacao, Tempo == FINISH)
factors = as.matrix(dados_prim[4:11])
response = as.matrix(dados_prim[3])

analyze.prim(factors, response, threshold.type=1,
             threshold=988229)

# Exibição - Filtrando as Variáveis a Observar no Gráfico
dadosplot = dplyr::filter(dados_simulacao, Tempo == FINISH) %>% select (Adoption_Rate, ContactRate, Adopters)

dadosplot = as.matrix(dadosplot)

names = colnames(dadosplot)

s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])

names(s) = names

# # Visualizando os Resultados

dados = dados_simulacao
x_axis_name = "Tempo"
y_axis_name = "Adopters"

# Visualizando Todas as Replicações
ggplot(dados_simulacao,
       aes(x=Tempo, y=Adopters, color=Replicacao, group=Replicacao)) + 
  geom_line() + 
  ylab("Adopters") + 
  xlab("Tempo") + guides(color=FALSE)




# Visualizando Todas as Replicações
ggplot(dados_simulacao,
       aes(x=Tempo, y=Adopters, color=Replicacao, group=Replicacao)) + 
  geom_line() + 
  ylab("Adopters") + 
  xlab("Tempo") + guides(color=FALSE)


# 

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
  title = "População",
  titlefont = f
)

plot_plotly = plot_ly(x = s$Adoption_Rate, y = s$ContactRate, z = s$Adopters) %>% add_surface() %>% layout(xaxis = x, yaxis = y, zaxis = z)


# Armazenando os Resultados
# write.csv(dados_simulacao, file = "dados_simulados_difusao.csv", row.names = FALSE)
