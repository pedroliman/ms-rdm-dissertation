# Um primeiro exemplo de https://github.com/JimDuggan/SDMR

# Loading Libraries
library(deSolve)
library(ggplot2)
require(gridExtra)
library(scales)
library(lhs)


##### Sampling #####
variaveis = 2
pontos = 100

# Obtendo um Hypercubo com as Variáveis que eu Quero
randomLHS <- randomLHS(pontos, variaveis)

# Transformando o Hypercubo em variáveis
var <- matrix(0, nrow=pontos, ncol=variaveis)

##### Amostragem #####
# Definindo Variáveis, mínimos e máximos:
variaveis = c("TaxaNascimento","TaxaMorte")
minimos = c(0.001,0.001)
maximos = c(0.05, 20)

# Obtendo a Amostra
Amostra = qunif(p=randomLHS,min=minimos, max=maximos)

plot(Amostra)





##### Setup Dinâmica de Sistemas ####

# Definindo Tempos da Simulação
START<-2015; FINISH<-2030; STEP<-0.25

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


# Rodando a Simulação às Ganhas


# Rodando a Simulação em todo o Ensemble

# Provavelmente eu deveria usar o apply ou um map para rodar a função várias vezes.

# Visualizando os Resultados
p1<-ggplot()+
  geom_line(data=o,aes(time,o$sPopulacao,color="Populacao"))+
  scale_y_continuous(labels = comma)+
  ylab("Stock")+
  xlab("Year") +
  labs(color="")+
  theme(legend.position="none")


p2<-ggplot()+
  geom_line(data=o,aes(time,o$Mortes,color="Mortes"))+
  geom_line(data=o,aes(time,o$Nascimentos,color="Nascimentos"))+
  scale_y_continuous(labels = comma)+
  ylab("Flows")+
  xlab("Year") +
  labs(color="")+
  theme(legend.position="none")

p3<-grid.arrange(p1, p2,nrow=2, ncol=1)