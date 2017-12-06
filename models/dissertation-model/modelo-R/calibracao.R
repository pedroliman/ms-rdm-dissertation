library(deSolve)
library(ggplot2)
library(gdata)
library(scales)
library(FME)
library(readxl)

dados_calibracao <- as.data.frame(read_xlsx(path = "dados_calibracao.xlsx", sheet = "Plan1")) 

variaveis_calibracao = names(dados_calibracao)

# Valores Iniciais dos parametros
parametros<-c(aPopulation=1000000, aWOMStrength = 1) 
lower<-c(1000, 0.3)
upper<-c(10000000, 3)


# Aqui é onde a calibração pode ser feita.

Fit<-modFit(p = parametros, f = getCost, modelo = modelo, dados_calibracao = dados_calibracao, lower=lower, upper=upper)

optPar1var = c(Fit$par)

optPar<-c(Fit$par)

cost = getCost(parametros = optPar, modelo = modelo, dados_calibracao = dados_calibracao)

cost$var

optMod <- solveWP(optPar)

# see http://www.inside-r.org/packages/cran/FME/docs/modFit

time_points<-seq(from=1, to=length(SIM_TIME),by=1/STEP)
optMod<-optMod[time_points,]

Fit$ssr

p1<-ggplot()+geom_point(data=dados_calibracao,size=1.5,aes(time,fIndustryOrderRate,colour="Data"))+
  geom_line(data=optMod,size=1,aes(x=time,y=fIndustryOrderRate,colour="Model"))+
  ylab("Demanda da Indústria")+
  xlab("Anos")+
  scale_y_continuous(labels = comma)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Dados",
                               "Modelo"))


p2<-ggplot()+geom_point(data=dados_calibracao,size=1.5,aes(time,sPrice1,colour="Data"))+
  geom_line(data=optMod,size=1,aes(x=time,y=sPrice1,colour="Model"))+
  ylab("Preço")+
  xlab("Anos")+
  scale_y_continuous(labels = comma)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Dados",
                               "Modelo"))
