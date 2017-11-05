# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
START<-0; FINISH<-20; STEP<-0.25

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)

# Número de Players no modelo
N_PLAYERS = 2



# Matriz de Variáveis que possuem valores no tempo global
matriz.variaveis.globais = matrix(simtime)

n_tempo = length(simtime)

nlinhas_matriz = nrow(matriz.variaveis.globais)

# Adcionando variável sReportedIndustryVolume
matriz.variaveis.globais = cbind(matriz.variaveis.globais, NA)

colnames(matriz.variaveis.globais) = c("Tempo", "sReportedIndustryVolume")

list.variaveis.globais = list(
  sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
  aExpectedIndustryDemand = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo)
)



##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
auxs    <- list(aDiscountRate = 0.04
                ,fChangeInPrice = rep(0, times = N_PLAYERS)
                ,aUnitVariableCost = rep(10, times = N_PLAYERS)
                ,aUnitFixedCost = rep(10, times = N_PLAYERS)
                ,aCapacity = rep(3, times = N_PLAYERS)
                ,aOrderShare = rep(0.5, times = N_PLAYERS)
                #,fIndustryOrderRate = 10
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
                ,aSwitchForShipmentsInForecast = 1
                ,aVolumeReportingDelay = rep(0.25, times = N_PLAYERS)
                ,aForecastHorizon = rep(1, times = N_PLAYERS)
                ,aCapacityAcquisitionDelay = 1
                ,aTimeForHistoricalVolume = 1
                )


##### VARIÁVEIS DE ENTRADA - ESTOQUES #####
stocks  <- c(
   sNPVProfit = rep(0, times = N_PLAYERS)
  ,sValueOfBacklog = rep(1, times = N_PLAYERS)
  ,sBacklog = rep(1, times = N_PLAYERS) 
  ,sInstalledBase = rep(50000, times = N_PLAYERS) # Este estoque possui uma fórmula, verificar como fazer aqui no R.
  ,sPrice = rep(9000, times = N_PLAYERS)
  ,sCumulativeAdopters = 100000 # Este estoque possui uma fórmula, verificar como fazer aqui no R.
  ,sReportedIndustryVolume = rep(10000, times = N_PLAYERS)            )

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
    sReportedIndustryVolume = stocks[(N_PLAYERS*6):(N_PLAYERS*6+1)]
    
    #Obtendo o número da linha no qual estou
    linha = (time * (n_tempo - 1)) / FINISH + 1
    
    list.variaveis.globais$sReportedIndustryVolume[linha,] <<- sReportedIndustryVolume
    
    # Gravando a Variável sReportedIndustryVolume no vetor global
    
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
    
    aIndustryShipments = sum(fShipments)
    
    aDeliveryDelay = sBacklog/fShipments
    
    ##### EXPECTED INDUSTRY DEMAND SECTOR #####
    
    aInitialDemandForecast = fReorderRate
    
    aIndustryVolume = max(aInitialDemandForecast,
                          aSwitchForShipmentsInForecast*aIndustryShipments+
                            (1-aSwitchForShipmentsInForecast)*fIndustryOrderRate)
    
    
    # Variavel com SMOOTH - Primeira Ordem: - Retirando o DT, o calculo funcionou corretamente!
    fsmooth_ReportedIndustryVolume = ((aIndustryVolume - sReportedIndustryVolume) / aVolumeReportingDelay) # * STEP # Multiplicando pelo step para ajustar o calculo.
    
    # Variavel com DELAY - A definição das constantes aqui devem ser alteradas se as condicoes iniciais do modelo mudarem
    # Esta implementacao considera que os delays sempre serao iguais. Se os delays nao forem iguais, deve-se encontrar outra forma de implementar os delays (talvez com a equacao multiplicativa 1*(time > tempodelay)
    if(time > aTimeForHistoricalVolume) {
      nlinhas_delay = aTimeForHistoricalVolume / STEP
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[linha-nlinhas_delay,]
    } else {
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[linha,]
    }
    
    aExpGrowthInVolume =  log(sReportedIndustryVolume/aLaggedIndustryVolume)/aTimeForHistoricalVolume
    
    
    aExpectedIndustryDemand = sReportedIndustryVolume*exp(aForecastHorizon*aCapacityAcquisitionDelay*aExpGrowthInVolume)
    
    list.variaveis.globais$aExpectedIndustryDemand[linha,] <<- aExpectedIndustryDemand
    
    # Mais uma variável com delay
    if(time > aCapacityAcquisitionDelay) {
      nlinhas_delay = aCapacityAcquisitionDelay / STEP
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[linha-nlinhas_delay,]
    } else {
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[linha,]
    }
    
    aForecastError = (aLaggedVolumeForecast - aIndustryVolume)/(1e-009+aIndustryVolume)
    
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
    
    d_sReportedIndustryVolume_dt = fsmooth_ReportedIndustryVolume
    
    ##### VARIÁVEIS RETORNADAS #####
    
    ## Parar se o tempo chegou ao fim.
    if(time == FINISH){
     # browser()
    }
    
    return (list(c(
                   d_NPVProfit_dt
                   ,d_ValueOfBacklog_dt
                   ,d_Backlog_dt
                   ,d_InstalledBase_dt
                   ,d_Price_dt
                   ,d_CumulativeAdopters_dt
                   ,d_sReportedIndustryVolume_dt
                   )
                 ,fReorderRate = fReorderRate
                 ,aIndustryShipments = aIndustryShipments
                 ,fIndustryOrderRate = fIndustryOrderRate
                 ,aIndustryVolume = aIndustryVolume
                 ,fDiscardRate = fDiscardRate
                 ,aDiscountFactor = aDiscountFactor
                 ,aDiscountRate = aDiscountRate
                 ,fNPVProfitChange = fNPVProfitChange
                 ,fNetIncome = fNetIncome
                 ,aNPVIndustryProfits = aNPVIndustryProfits
                 ,aInitialDemandForecast = aInitialDemandForecast
                 ,aLaggedVolumeForecast = aLaggedVolumeForecast))   
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



