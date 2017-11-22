library(deSolve)
library(ggplot2)
library(gdata)
library(scales)
library(FME)
library(readxl)

dados_calibracao <- read_xlsx(path = "dados_calibracao.xlsx", sheet = "Plan1")

dados_calibracao <- as.data.frame(dados_calibracao)

variaveis_calibracao = names(dados_calibracao)

# dados_calibracao <- read.xls("D:/dev/SDMR/models/07Chapter/R/WorldPopulation.xlsx",stringsAsFactors=F)

WP_INIT<-dados_calibracao[1,"Population"]

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{  
    
    fPopulationAdded<-Population * aGrowthFraction
    
    fPopulationRemoved <- Population * aDeclineFraction
    
    dP_dt  <- fPopulationAdded - fPopulationRemoved
    
    return (list(c(dP_dt)))   
  })
}

solveWP <- function(pars){
  
  # All the stocks are initialised here...
  
  # Matriz de Variáveis que possuem valores no tempo global
  matriz.variaveis.globais <<- matrix(simtime)
  
  n_tempo = length(simtime)
  
  nlinhas_matriz = nrow(matriz.variaveis.globais)
  
  # Adcionando variável sReportedIndustryVolume
  matriz.variaveis.globais <<- cbind(matriz.variaveis.globais, NA)
  
  colnames(matriz.variaveis.globais) <<- c("Tempo", "sReportedIndustryVolume")
  
  list.variaveis.globais <<- list(
    sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
    aExpectedIndustryDemand = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo)
  )
  
  # Carregando Variáveis de Output do Ithink para Comparação
  arquivo_excel_stocks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_stocks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"))
  arquivo_excel_checks = carregar_inputs(arquivo_de_inputs = "../modelo-ithink/dados_ithink_excel_checks.xlsx", abas_a_ler = c("Plan1"), nomes_inputs = c("ResultadosIthink"))
  
  dados_ithink_stocks  = arquivo_excel_stocks$ResultadosIthink %>% dplyr::select(-Months)
  dados_ithink_checks  = arquivo_excel_checks$ResultadosIthink %>% dplyr::select(-Months)
  
  variaveis_ithink_stocks = names(dados_ithink_stocks)
  variaveis_ithink_checks = names(dados_ithink_checks)
  
  ##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
  auxs    <- list(aDiscountRate = 0.04
                  ,aNormalDeliveryDelay = rep(0.25, times = N_PLAYERS)
                  ,aSwitchForCapacity = 1
                  # Vamos testar apenas um parâmetro por enquanto
                  ,aFractionalDiscardRate = unname(pars["aFractionalDiscardRate"])
                  ,aInitialDiffusionFraction = 0.001
                  ,aReferencePrice = 1000
                  ,aReferenceIndustryDemandElasticity = 0.2
                  ,aReferencePopulation = 60000000
                  ,aInnovatorAdoptionFraction = 0.001
                  ,aWOMStrength = 1
                  ,aPopulation = 100000000
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
                  ,aMinimumEfficientScale = rep(100000, times = N_PLAYERS)
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
  )
  
  
  ##### VARIÁVEIS DE ENTRADA - ESTOQUES #####
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
  
  
  #stocks  <- c(Population=WP_INIT)
  
  # Adicinar aos auxiliares anteriores aqueles que eu quero calibrar. Por enquanto, nenhum.
  
  resultado_completo = data.frame(ode(y=stocks, simtime, func = modelo, 
                                      parms=auxs, method="euler"))
  
  resultado_completo[variaveis_calibracao]
}

getCost<-function(p){
  out<-solveWP(p)
  #http://www.inside-r.org/packages/cran/FME/docs/modCost
  cost <- modCost(obs=dados_calibracao,model=out)
  #cat(str(cost))
  return(cost)
}

# Valores Iniciais dos parametros
pars<-c(aFractionalDiscardRate=0.1) 
lower<-c(0.0)
upper<-c(0.3)


Fit<-modFit(p=pars,f=getCost,lower=lower,upper=upper)

optPar<-c(Fit$par)

cost = getCost(optPar)

optMod <- solveWP(optPar)

# see http://www.inside-r.org/packages/cran/FME/docs/modFit

time_points<-seq(from=1, to=length(simtime),by=1/STEP)
optMod<-optMod[time_points,]


Fit$ssr

p1<-ggplot()+geom_point(data=dados_calibracao,size=1.5,aes(time,fIndustryOrderRate,colour="Data"))+
  geom_line(data=optMod,size=1,aes(x=time,y=fIndustryOrderRate,colour="Model"))+
  ylab("People")+
  xlab("Year")+
  scale_y_continuous(labels = comma)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Data",
                               "Model"))
p1



