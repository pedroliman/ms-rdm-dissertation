library(deSolve)
library(ggplot2)
library(gdata)
library(scales)
library(FME)
library(readxl)

## Inicializar variaveis da simulacao aqui:
START<-0; FINISH<-10; STEP<-0.0625

VERIFICAR_STOCKS = FALSE

VERIFICAR_CHECKS = FALSE

CHECK_PRECISION = 0.00001

BROWSE_ON_DIFF = FALSE

# Vetor de Tempos
SIM_TIME <- seq(START, FINISH, by=STEP)

dados_calibracao <- as.data.frame(read_xlsx(path = "dados_calibracao.xlsx", sheet = "Plan1")) 

variaveis_calibracao = names(dados_calibracao)

# Valores Iniciais dos parametros
parametros<-c(aPopulation=1000000, aWOMStrength = 1) 
lower<-c(1000, 0.3)
upper<-c(10000000, 3)



solve_modelo_dissertacao <- function(parametros, modelo){
  
  # Número de Players no modelo
  N_PLAYERS = 2
  
  # All the stocks are initialised here...
  
  n_tempo = length(SIM_TIME)

  list.variaveis.globais <<- list(
    sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
    aExpectedIndustryDemand = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo)
  )
  
  ##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
  auxs    <- list(aDiscountRate = 0.04
                  ,aNormalDeliveryDelay = rep(0.25, times = N_PLAYERS)
                  ,aSwitchForCapacity = 1
                  # Vamos testar apenas um parâmetro por enquanto
                  ,aFractionalDiscardRate = 0.1 # unname(pars["aFractionalDiscardRate"]) # Original 0.1
                  ,aInitialDiffusionFraction = 0.001
                  ,aReferencePrice = 1000
                  ,aReferenceIndustryDemandElasticity = 0.2
                  ,aReferencePopulation = 60000000
                  ,aInnovatorAdoptionFraction = 0.001
                  ,aWOMStrength = unname(parametros["aWOMStrength"]) # unname(pars["aWOMStrength"]) # Original 1
                  ,aPopulation = unname(parametros["aPopulation"]) #100000000 # Original Sterman: 100000000
                  ,aUnitsPerHousehold = 1
                  ,aSwitchForShipmentsInForecast = 0
                  ,aVolumeReportingDelay = rep(0.25, times = N_PLAYERS)
                  ,aForecastHorizon = rep(1, times = N_PLAYERS)
                  ,aCapacityAcquisitionDelay = 1
                  ,aTimeForHistoricalVolume = 1
                  # Market Sector
                  ,aReferenceDeliveryDelay = 0.25
                  ,aSensOfAttractToAvailability = -4
                  ,aSensOfAttractToPrice = -8
                  # Learning Curve Params
                  ,aLCStrength = rep(0.7, times = N_PLAYERS)
                  ,aInitialProductionExperience = rep(1e+007, times = N_PLAYERS)
                  ,aRatioOfFixedToVarCost = rep(3, times = N_PLAYERS)
                  ,aInitialPrice = rep(1000, times = N_PLAYERS)
                  ,aNormalProfitMargin = rep(0.2, times = N_PLAYERS)
                  ,aNormalCapacityUtilization = rep(0.8, times = N_PLAYERS)
                  #Target Capacity Sector
                  ,aMinimumEfficientScale = rep(100, times = N_PLAYERS) # Original 100000
                  ,aDesiredMarketShare = rep(0.5, times = N_PLAYERS)
                  ,aWeightOnSupplyLine= rep(1, times = N_PLAYERS)
                  ,aSwitchForCapacityStrategy = rep(1, times = N_PLAYERS)
                  ,aTimeToPerceiveCompTargetCapacity = rep(0.25, times = N_PLAYERS)
                  # Price Sector
                  ,aPriceAdjustmentTime = 0.25
                  ,aSensOfPriceToCosts = rep(1, times = N_PLAYERS)
                  ,aSensOfPriceToDSBalance = rep(0.25, times = N_PLAYERS)
                  ,aSensOfPriceToShare = rep(-0.1, times = N_PLAYERS)
                  # Capacity Sector
                  ,aSwitchForPerfectCapacity = 0
                  # A Initial Price
                  ,aInitialPrice = rep(1000, times = N_PLAYERS)
  )
  
  
  ##### VARIÁVEIS DE ENTRADA - ESTOQUES INICIAIS, SEM AJUSTES #####
  
  # Informando Estoques Iniciais, sem ajustes, apenas para calcular o primeiro tempo.
  stocks  <- c(
    sNPVProfit = rep(0, times = N_PLAYERS)
    ,sValueOfBacklog = rep(12738001, times = N_PLAYERS)
    ,sBacklog = rep(12738, times = N_PLAYERS) 
    ,sInstalledBase = rep(30000, times = N_PLAYERS) # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    ,sPrice = rep(1000, times = N_PLAYERS)
    ,sCumulativeAdopters = 60000 # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    ,sReportedIndustryVolume = rep(101904, times = N_PLAYERS)
    ,sCumulativeProduction = rep(1e+007, times = N_PLAYERS) # Este estoque possui formula
    ,sPerceivedCompTargetCapacity = rep(63690, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity1 = rep(63690, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity2 = rep(63690, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity3 = rep(63690, times = N_PLAYERS) # Este estoque possui formula
  ) 
  
  # Calculando estoques para o t0.
  estoques_calculados = modelo(time = 0, stocks = stocks, auxs = auxs, modo = "inicial")
  
  # Substituindo estoques no t0
  stocks  <- c(
    sNPVProfit = rep(0, times = N_PLAYERS)
    ,sValueOfBacklog = unname(estoques_calculados$ValueOfBacklogIni)
    ,sBacklog = unname(estoques_calculados$BacklogIni)
    ,sInstalledBase = rep(unname(estoques_calculados$InstalledBaseIni), times = N_PLAYERS)
    ,sPrice = unname(auxs$aInitialPrice)
    ,sCumulativeAdopters = unname(estoques_calculados$CumulativeAdoptersIni)
    ,sReportedIndustryVolume = rep(unname(estoques_calculados$ReportedIndustryVolumeIni), times = N_PLAYERS)
    ,sCumulativeProduction = unname(estoques_calculados$CumulativeProductionIni)
    ,sPerceivedCompTargetCapacity = unname(estoques_calculados$PerceivedCompTargetCapacityIni)
    ,sSmoothCapacity1 = unname(estoques_calculados$CapacityIni)
    ,sSmoothCapacity2 = unname(estoques_calculados$CapacityIni)
    ,sSmoothCapacity3 = unname(estoques_calculados$CapacityIni)
  ) 
  
  resultado_completo = data.frame(ode(y=stocks, SIM_TIME, func = modelo, 
                                      parms=auxs, method="euler"))
  
  resultado_completo[variaveis_calibracao]
}

getCost<-function(parametros, modelo, dados_calibracao){
  
  output_modelo <- solve_modelo_dissertacao(parametros, modelo)
  #http://www.inside-r.org/packages/cran/FME/docs/modCost
  
  cost <- modCost(obs=dados_calibracao, model=output_modelo)
  
  return(cost)
  
}


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
p1
