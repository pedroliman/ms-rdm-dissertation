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
pontos = 100

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
START<-2015; FINISH<-2025; STEP<-0.125

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)


# Criando Estoques (na mão em um primeiro momento).
auxs    <- c(aAdvertisingEffectiveness= 0.01, aContactRate= 100, aAdoptionFraction= 0.02, aTotalPopulation= 1000000)

class(ensemble[,1])

# A ORDEM AQUI DEVE SER A MESMA DA ORDEM DE SAÍDA DO MODELO!!!!!!!
stocks  <- c(sPotentialAdopters=999990, sAdopters=10)

##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    aAdoption_from_Advertising = aAdvertisingEffectiveness * sPotentialAdopters
    
    aAdoption_from_Word_of_Mouth = aContactRate * sAdopters *  ((sPotentialAdopters)/(aTotalPopulation)) * aAdoptionFraction  # {people/year}
    
    fAdoption_Rate = aAdoption_from_Advertising + aAdoption_from_Word_of_Mouth # {people/year}
    
    d_sPotentialAdopters_dt = - fAdoption_Rate
    
    d_sAdopters_dt = fAdoption_Rate
    
    return (list(c(d_sPotentialAdopters_dt, d_sAdopters_dt),
                 AdvertisingEffectiveness = aAdvertisingEffectiveness,
                 ContactRate = aContactRate,
                 AdoptionFraction = aAdoptionFraction,
                 TotalPopulation = aTotalPopulation,
                 Adoption_from_Advertising = aAdoption_from_Advertising,
                 Adoption_from_Word_of_Mouth = aAdoption_from_Word_of_Mouth,
                 Adoption_Rate = fAdoption_Rate))   
  })
}


# Rodando a Simulação (uma vez)
o<-data.frame(ode(y=stocks, times=simtime, func = modelo, 
                  parms=ensemble[1,], method="euler"))

nlinhas = nrow(o)

ncolunas = ncol(o)+1

# Montando uma matriz com todos os dados para a simulação
dados_simulacao = matrix(nrow = pontos*nlinhas, ncol = ncolunas)

# J é o índice dos dados simulados
j = 1
print("Rodando Interações.")
# Rodando a Simulacao Em todo o Ensemble
for (i in 1:nrow(ensemble)) {
  # Começando a Rodar
  #print(paste("Rodando Iteracao",i))
  
  resultados_simulacao = ode(y=stocks, times=simtime, func = model, 
                             parms=ensemble[i,], method="euler")
  linhas = nrow(resultados_simulacao)
  l_inicial = j
  l_final = j + linhas-1
  dados_simulacao[l_inicial:l_final,1:ncolunas-1] = resultados_simulacao
  dados_simulacao[l_inicial:l_final,ncolunas] = i
  j = j + linhas
  }

# Nomeando o Dataframe de Saída
nomes_variaveis_final = c("Tempo", "PotentialAdopters", "Adopters", "AdvEffectiveness", "ContactRate", "AdoptionFraction", "TotalPopulation", "Adoption_From_Advertising", "Adoption_From_Word_of_Mouth", "Adoption_Rate", "Replicacao")

colnames(dados_simulacao) = nomes_variaveis_final

dados_simulacao = as.data.frame(dados_simulacao)
names(dados_simulacao) = nomes_variaveis_final

dadosplot = dplyr::filter(dados_simulacao, Tempo == FINISH) %>% select (Adoption_Rate, ContactRate, Adopters)

dadosplot = as.matrix(dadosplot)

names = colnames(dadosplot)

s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])

names(s) = names

# # Visualizando os Resultados

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

# plot_plotly = plot_ly(x = s$Adoption_Rate, y = s$ContactRate, z = s$Adopters) %>% add_surface() %>% layout(xaxis = x, yaxis = y, zaxis = z)


# Armazenando os Resultados
# write.csv(dados_simulacao, file = "dados_simulados_difusao.csv", row.names = FALSE)
