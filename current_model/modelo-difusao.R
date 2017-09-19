# Este exemplo é baseado no modelo de difusão do capítulo 6 do livro do Morecroft.

# Loading Libraries
library(deSolve)
library(ggplot2)
require(gridExtra)
library(scales)
library(lhs)
library(dplyr)
library(plotly)
library(akima)


## Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')


##### Amostragem #####
# Definindo Variáveis, mínimos e máximos:
# Neste exemplo serão Variáveis Aleatórias:
variaveis = c("aAdvertisingEffectiveness","aContactRate","aAdoptionFraction","aTotalPopulation","siniPotentialAdopters")

# Carregando Inputs
inputs = carregar_inputs()

#Obtendo DataFrame de Parâmetros
params = inputs$Parametros

##### Sampling #####
nvar = length(params$Variavel)
pontos = 200

# Obtendo um Hypercubo com as Variáveis que eu quero
randomLHS <- randomLHS(pontos, nvar)

p = as.data.frame(randomLHS)
min = as.vector(params$Min)
max = as.vector(params$Max)
variaveis = as.vector(params$Variavel)

# Transformando o Hypercubo em variáveis
# var <- matrix(nrow=pontos, ncol=variaveis)
ensemble = matrix(nrow = pontos, ncol = nvar)

# Montando o Ensemble
for (var in variaveis) {
  i = which(x = variaveis == var)
  ensemble[,i] = qunif(p = randomLHS[,i], min = min[i], max = max[i])
}
colnames(ensemble) = variaveis

##### Setup Dinâmica de Sistemas ####

# Definindo Tempos da Simulação
START<-2015; FINISH<-2020; STEP<-0.5

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)


# Criando Estoques 
stocks  <- c(sPopulacao=10000)
auxs    <- c(aTaxaNascimento=0.08, aTaxaMorte=0.03)


##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    
    fNascimentos<-sPopulacao*aTaxaNascimento
    
    fMortes<-sPopulacao*aTaxaMorte
    
    dPopulacao_dt <- fNascimentos - fMortes
    
    return (list(c(dPopulacao_dt),
                 Nascimentos=fNascimentos, Mortes=fMortes,
                 TaxaNascimentos=aTaxaNascimento,TaxaMortes=aTaxaMorte))   
  })
}


# Rodando a Simulação (uma vez)
o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                  parms=auxs, method="euler"))

# Montando uma matriz com todos os dados para a simulação
dados_simulacao = matrix(nrow = pontos*(1+(FINISH - START)/STEP), ncol = 7)


# J é o índice dos dados simulados
j = 1
# Rodando a Simulacao Em todo o Ensemble
for (i in 1:nrow(ensemble)) {
  # Começando a Rodar
  print(paste("Rodando Iteracao",i))
  dados_simulacao[j:((j+((FINISH - START)/STEP))),1:6] = ode(y=stocks, times=simtime, func = model, 
                               parms=ensemble[i,], method="euler")
  dados_simulacao[j:(j+((FINISH - START)/STEP)),7] = i
  j = j + 1 + ((FINISH - START)/STEP)
  }

# Nomeando o Dataframe de Saída
nomes_variaveis_final = c("Tempo", "Populacao", "Nascimentos", "Mortes", "TaxaNascimento", "TaxaMorte", "Replicacao")

colnames(dados_simulacao) = nomes_variaveis_final

dados_simulacao = as.data.frame(dados_simulacao)
names(dados_simulacao) = nomes_variaveis_final

dadosplot = dplyr::filter(dados_simulacao, Tempo == 2020) %>% select (TaxaNascimento, TaxaMorte, Populacao)

dadosplot = as.matrix(dadosplot)

names = colnames(dadosplot)

s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])

names(s) = names

# # Visualizando os Resultados

# Visualizando Todas as Replicações
ggplot(dados_simulacao,
       aes(x=Tempo, y=Populacao, color=Replicacao, group=Replicacao)) + 
  geom_line() + 
  ylab("Populacao") + 
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

plot_ly(x = s$TaxaNascimento, y = s$TaxaMorte, z = s$Populacao) %>% add_surface() %>% layout(xaxis = x, yaxis = y, zaxis = z)


# Armazenando os Resultados
write.csv2(dados_simulacao, file = "dados_simulados.csv")