library(deSolve)
library(ggplot2)
library(gdata)
library(scales)
library(FME)
library(readxl)
# Todos os Dados simulados estão no objeto results, e os dados ano a ano estão no objeto DadosSimulados

variaveis_calibracao = c("fIndustryOrderRate")

# Valores Iniciais dos parametros
#parametros<-c(aPopulation=1000000, aWOMStrength = 1) 
#lower<-c(1000, 0.3)
#upper<-c(10000000, 3)


arquivo_parametros = "./rodada1/params.xlsx"

View(parametros_completos)

parametros_completos = readxl::read_xlsx(arquivo_parametros, sheet = "params")

parametros = t(parametros_completos[,"CenarioBase"])[1,]

names(parametros) = as.matrix(parametros_completos[,1])

lower = t(parametros_completos[,"Min"])[1,] 
upper = t(parametros_completos[,"Max"])[1,]

params = parametros

# Aqui é onde a calibração pode ser feita.

Fit<-modFit(p = params, f = getCost, modelo = modelo, dados_calibracao = dados_calibracao, lower=lower, upper=upper)

Fit<-modFit(p = params, f = getCost, modelo = modelo, dados_calibracao = dados_calibracao, lower=lower, upper=upper)

getCost(p = params, modelo = modelo, dados_calibracao = dados_calibracao)

Fit = FME::modFit(f = solve_modelo_dissertacao(parametros = params, modelo = modelo, simtime = SIM_TIME), p = params)

getCost<-function(parametros, modelo, dados_calibracao)



optPar1var = c(Fit$par)

optPar<-c(Fit$par)

cost = getCost(parametros = optPar, modelo = modelo, dados_calibracao = dados_calibracao)

cost$var

optMod <- solveWP(optPar)

# see http://www.inside-r.org/packages/cran/FME/docs/modFit


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
