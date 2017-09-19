# Um primeiro exemplo de https://github.com/JimDuggan/SDMR

# Loading Libraries
library(deSolve)
library(ggplot2)
require(gridExtra)
library(scales)
library(lhs)
library(dplyr)


##### Sampling #####
nvar = 2
pontos = 10

# Obtendo um Hypercubo com as Variáveis que eu quero
randomLHS <- randomLHS(pontos, nvar)

# Transformando o Hypercubo em variáveis
var <- matrix(nrow=pontos, ncol=variaveis)

##### Amostragem #####
# Definindo Variáveis, mínimos e máximos:
variaveis = c("aTaxaNascimento","aTaxaMorte")
p = as.data.frame(randomLHS)
min = c(0.001,0.001)
max = c(0.05, 0.05)

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


# Rodando a Simulação uma vez
#o<-data.frame(ode(y=stocks, times=simtime, func = model, 
#                  parms=ensemble[1,], method="euler"))


# J é o índice dos dados simulados
j = 1
# Rodando a Simulacao Em todo o Ensemble
for (i in 1:nrow(ensemble)) {
  # Começando a Rodar
  print(paste("Rodando Iteracao",i))
  dados_simulacao[j:((j+((FINISH - START)/STEP))),1:6] = ode(y=stocks, times=simtime, func = model, 
                               parms=ensemble[1,], method="euler")
  dados_simulacao[j:(j+((FINISH - START)/STEP)),7] = i
  j = j + 1 + ((FINISH - START)/STEP)
  }

# Nomeando o Dataframe de Saída
nomes_variaveis_final = c("Tempo", "Populacao", "Nascimentos", "Mortes", "TaxaNascimento", "TaxaMorte", "Replicacao")

colnames(dados_simulacao) = nomes_variaveis_final

dados_simulacao = as.data.frame(dados_simulacao)
names(dados_simulacao) = nomes_variaveis_final

# Resumindo Dados Finais (continuar daqui...Mas já está bom demais)
replicacoes = group_by(dados_simulacao, Replicacao)

write.csv2(dados_simulacao, file = "dados_simulados.csv")

# 
# # Visualizando os Resultados
# p1<-ggplot()+
#   geom_line(data=o,aes(time,o$sPopulacao,color="Populacao"))+
#   scale_y_continuous(labels = comma)+
#   ylab("Stock")+
#   xlab("Year") +
#   labs(color="")+
#   theme(legend.position="none")
# 
# 
# p2<-ggplot()+
#   geom_line(data=o,aes(time,o$Mortes,color="Mortes"))+
#   geom_line(data=o,aes(time,o$Nascimentos,color="Nascimentos"))+
#   scale_y_continuous(labels = comma)+
#   ylab("Flows")+
#   xlab("Year") +
#   labs(color="")+
#   theme(legend.position="none")
# 
# p3<-grid.arrange(p1, p2,nrow=2, ncol=1)