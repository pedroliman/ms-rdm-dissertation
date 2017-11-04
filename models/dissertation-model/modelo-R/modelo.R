# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
START<-0; FINISH<-20; STEP<-0.25

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)

# Número de Players no modelo
N_PLAYERS = 2

##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
auxs    <- list(aDiscountRate = 0.04
                ,fChangeInPrice = rep(0, times = N_PLAYERS)
                ,aUnitVariableCost = rep(10, times = N_PLAYERS)
                ,aUnitFixedCost = rep(10, times = N_PLAYERS)
                ,aCapacity = rep(3, times = N_PLAYERS)
                ,aOrderShare = rep(0.5, times = N_PLAYERS)
                ,fIndustryOrderRate = 10
                ,aNormalDeliveryDelay = rep(0.25, times = N_PLAYERS)
                ,aSwitchForCapacity = 1
                ,aFractionalDiscardRate = 0.1
                ,aInitialDiffusionFraction = 0.001
                ,aReferencePrice = 1000
                ,aReferenceIndustryDemandElasticity = 1000
                ,aReferencePopulation = 60000000
                ,aInnovatorAdoptionFraction = 0.001
                ,aWOMStrength = 1
                ,aPopulation = 100000000
                ,aUnitsPerHousehold = 1
                )






##### VARIÁVEIS DE ENTRADA - ESTOQUES #####
stocks  <- c(
   sNPVProfit = rep(0, times = N_PLAYERS)
  ,sValueOfBacklog = rep(1, times = N_PLAYERS)
  ,sBacklog = rep(1, times = N_PLAYERS)
  ,sInstalledBase = rep(1, times = N_PLAYERS)
  ,sPrice = rep(9000, times = N_PLAYERS)
  ,sCumulativeAdopters = 100000 # Este estoque possui uma fórmula, verificar como fazer aqui no R.
             )

##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    ##### VETORIZANDO ESTOQUES #####
    
    #Estoques Vetorizados = substituindo estoques pela forma vetorizada (pra que seja possivel formular equações de forma mais simples).
    # Esta implementação tem por objetivo não gerar a necessidade de referenciar os estoque spelo seu nome único
    sNPVProfit = stocks[(N_PLAYERS*0+1):(N_PLAYERS*1)]
    sValueOfBacklog = stocks[(N_PLAYERS*1+1):(N_PLAYERS*2)]
    sBacklog = stocks[(N_PLAYERS*2+1):(N_PLAYERS*3)]
    sInstalledBase = stocks[(N_PLAYERS*3+1):(N_PLAYERS*4)]
    sPrice = stocks[(N_PLAYERS*4+1):(N_PLAYERS*5)]
    sCumulativeAdopters = stocks[(N_PLAYERS*5+1)]
    
    ##### DIFFUSION SECTOR #####
    aDemandCurveSlope = (aReferencePopulation*aReferenceIndustryDemandElasticity)/(aReferencePrice)
    
    aLowestPrice = min(sPrice)
    
    aIndustryDemand = min(
      aPopulation,
      aReferencePopulation * max(
        0,
        1 + aDemandCurveSlope * (aLowestPrice - aReferencePrice) / aReferencePopulation
      )
    )
    
    aInitialCumulativeAdopters = aInitialDiffusionFraction * aIndustryDemand
    
    aNonAdopters = aIndustryDemand - sCumulativeAdopters
    
    fAdoptionRate = aNonAdopters * (aInnovatorAdoptionFraction + aWOMStrength*sCumulativeAdopters/aPopulation)
    
    ##### ORDERS SECTOR - PT 1 #####
    
    fDiscardRate = sInstalledBase * aFractionalDiscardRate
    
    ##### INDUSTRY DEMAND SECTOR #####
    
    fReorderRate = sum(fDiscardRate)
    
    aInitialOrderRate = aUnitsPerHousehold * fAdoptionRate
    
    fIndustryOrderRate = fReorderRate + aInitialOrderRate
    
    ##### ORDERS SECTOR - PT 2 #####
    
    fOrders = fIndustryOrderRate * aOrderShare
    
    aDesiredShipments = sBacklog/aNormalDeliveryDelay
    
    fShipments = aSwitchForCapacity * min(aDesiredShipments, aCapacity) + (1-aSwitchForCapacity) * aDesiredShipments
    
    aDeliveryDelay = sBacklog/fShipments
    
    
    
    
    ##### NET INCOME SECTOR #####
    
    aDiscountFactor = exp(-aDiscountRate*time)
    
    fValueOfNewOrders = fOrders * sPrice
    
    aAveragePriceOfOrderBook = sValueOfBacklog / sBacklog
    
    fRevenue = fShipments * aAveragePriceOfOrderBook
    
    aVariableCost = fShipments * aUnitVariableCost
    
    aFixedCost = aCapacity * aUnitFixedCost
    
    fCost = aFixedCost + aVariableCost
    
    fNetIncome = fRevenue - fCost
    
    fNPVProfitChange = fNetIncome * aDiscountFactor
    
    aNPVIndustryProfits = sum(sNPVProfit)
    
    ##### ESTOQUES #####
    
    d_NPVProfit_dt = fNPVProfitChange
    
    d_ValueOfBacklog_dt = fValueOfNewOrders - fRevenue
    
    d_Backlog_dt = fOrders - fShipments
    
    d_InstalledBase_dt = fShipments - fDiscardRate
    
    d_Price_dt = fChangeInPrice
    
    d_CumulativeAdopters_dt = fAdoptionRate
    
    ##### VARIÁVEIS RETORNADAS #####
    
    return (list(c(
                   d_NPVProfit_dt
                   ,d_ValueOfBacklog_dt
                   ,d_Backlog_dt
                   ,d_InstalledBase_dt
                   ,d_Price_dt
                   ,d_CumulativeAdopters_dt
                   )
                 ,aDiscountFactor = aDiscountFactor
                 ,aDiscountRate = aDiscountRate
                 ,fNPVProfitChange = fNPVProfitChange
                 ,fNetIncome = fNetIncome
                 ,aNPVIndustryProfits = aNPVIndustryProfits))   
  })
}


# Nomeando o Dataframe de Saída
nomes_variaveis = c("Tempo", "d_NPVProfit_dt", "aDiscountFactor", "aDiscountRate", "fNPVProfitChange", "fNetIncome", "aNPVIndustryProfits")


# Inicializando um list com Tudo o que é necessário
sdmodel = list(
  Start = START,
  Finish = FINISH,
  Step = STEP,
  SimTime = simtime,
  Auxs = auxs,
  Stocks = stocks,
  Modelo = modelo,
  Variaveis = nomes_variaveis
)


pedro.smooth1 = function(input, delay, valor_inicial) {
  change_smooth = (input - 
}

