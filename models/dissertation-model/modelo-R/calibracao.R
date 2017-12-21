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

# Rodando a Simulação com os Parâmetros do Sterman, rodando uma vez apenas.
arquivo_parametros = "./calibracao/params_calibracao.xlsx"

parametros_completos = readxl::read_xlsx(arquivo_parametros, sheet = "params")

parametros_cenariobase = t(parametros_completos[,"CenarioBase"])[1,]

names(parametros_cenariobase) = as.matrix(parametros_completos[,1])

params = parametros_cenariobase

# Aqui é onde a calibração pode ser feita.
Fit<-modFit(p = params, f = getCost, modelo = modelo, dados_calibracao = dados_calibracao, upper = +Inf, lower = -Inf)

Fit = FME::modFit(f = solve_modelo_dissertacao, parametros = params, modelo = modelo, simtime = SIM_TIME, p = params)

teste = modFit(getCost, params, modelo = modelo, dados_calibracao = dados_calibracao)

upper = params + 0.5 * abs(params)

lower = params - 0.5 * abs(params)

teste = modFit(f = getCost, p = params, modelo = sdmodel$Modelo, dados_calibracao = dados_calibracao, upper = upper, lower = lower)


teste$par == params

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
