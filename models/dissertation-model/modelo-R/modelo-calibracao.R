# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
library(dplyr)
START<-10; FINISH<-10; STEP<-0.0625

VERIFICAR_STOCKS = FALSE

VERIFICAR_CHECKS = FALSE

CHECK_PRECISION = 0.00001

BROWSE_ON_DIFF = FALSE

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)

# Número de Players no modelo
N_PLAYERS = 2

##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs, modo = "completo"){
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
    sCumulativeProduction = stocks[(N_PLAYERS*7):(N_PLAYERS*7+1)]
    sPerceivedCompTargetCapacity = stocks[(N_PLAYERS*8):(N_PLAYERS*8+1)]
    sSmoothCapacity1 = stocks[(N_PLAYERS*9):(N_PLAYERS*9+1)]
    sSmoothCapacity2 = stocks[(N_PLAYERS*10):(N_PLAYERS*10+1)]
    sSmoothCapacity3 = stocks[(N_PLAYERS*11):(N_PLAYERS*11+1)]
    
    #Obtendo o número da linha no qual estou
    linha = (time * (n_tempo - 1)) / FINISH + 1
    
    list.variaveis.globais$sReportedIndustryVolume[linha,] <<- sReportedIndustryVolume
    
    # Gravando a Variável sReportedIndustryVolume no vetor global
    
    ##### DIFFUSION SECTOR #####
    aDemandCurveSlope = (- aReferencePopulation * aReferenceIndustryDemandElasticity )/ ( aReferencePrice )
    
    aLowestPrice = min(sPrice)
    
    aIndustryDemand = min(
      aPopulation,
      aReferencePopulation * max(
        0,
        1 + aDemandCurveSlope * (aLowestPrice - aReferencePrice) / aReferencePopulation
      )
    )
    
    checkIndustryDemand = aIndustryDemand
    
    aInitialCumulativeAdopters = aInitialDiffusionFraction * aIndustryDemand
    
    aNonAdopters = aIndustryDemand - sCumulativeAdopters
    
    checkNonAdopters = aNonAdopters
    
    # Ajuste temporário: Colocar o adoption Rate como Fluxo apenas positivo.
     
    fAdoptionRate = max(0, 
                        aNonAdopters * (aInnovatorAdoptionFraction + aWOMStrength * sCumulativeAdopters/aPopulation)) 
    
    checkAdoptionRate = fAdoptionRate
    
    ##### ORDERS SECTOR - PT 1 #####
    
    fDiscardRate = sInstalledBase * aFractionalDiscardRate
    
    ##### INDUSTRY DEMAND SECTOR #####
    
    fReorderRate = sum(fDiscardRate)
    
    aInitialOrderRate = aUnitsPerHousehold * fAdoptionRate
    
    fIndustryOrderRate = fReorderRate + aInitialOrderRate
    
    checkIndustryOrderRate = fIndustryOrderRate
    
    ##### ORDERS SECTOR - PT 2 #####
    
    aDesiredShipments = sBacklog / aNormalDeliveryDelay
    
    ### CAPACITY SECTOR - PT 1 ####
    
    aCapacity = aSwitchForPerfectCapacity * (aDesiredShipments / aNormalCapacityUtilization) + (1-aSwitchForPerfectCapacity) * sSmoothCapacity3
    
    aNormalProduction = aCapacity * aNormalCapacityUtilization
    
    aIndustryNormalProduction = sum(aNormalProduction)
    
    ##### ORDERS SECTOR - PT 3 #####
    
    fShipments = aSwitchForCapacity * pmin(aDesiredShipments, aCapacity) + (1-aSwitchForCapacity) * aDesiredShipments
    
    aCapacityUtilization = fShipments / aCapacity
    
    aIndustryShipments = sum(fShipments)
    
    aMarketShare = fShipments / aIndustryShipments
    
    aDeliveryDelay = sBacklog / fShipments
    
    checkIndustryShipments = aIndustryShipments
    
    ##### MARKET SECTOR #####
    
    aAttractivenessFromAvailability = exp(aSensOfAttractToAvailability*(aDeliveryDelay/aReferenceDeliveryDelay))
    
    aAttractivenessFromPrice = exp(aSensOfAttractToPrice*(sPrice/aReferencePrice))
    
    aAttractiveness = aAttractivenessFromAvailability * aAttractivenessFromPrice
    
    aTotalAttractiveness = sum(aAttractiveness)
    
    aOrderShare = aAttractiveness / aTotalAttractiveness
    
    ##### ORDERS SECTOR - PT 3 #####
    
    fOrders = fIndustryOrderRate * aOrderShare
    
    checkOrders = sum(fOrders)
    
    ##### EXPECTED INDUSTRY DEMAND SECTOR #####
    
    aInitialDemandForecast = fReorderRate
    
    aIndustryVolume = pmax(aInitialDemandForecast,
                          aSwitchForShipmentsInForecast*aIndustryShipments+
                            (1-aSwitchForShipmentsInForecast)*fIndustryOrderRate)
    
    
    # Variavel com SMOOTH - Primeira Ordem: - Retirando o DT, o calculo funcionou corretamente!
    fsmooth_ReportedIndustryVolume = ((aIndustryVolume - sReportedIndustryVolume) / aVolumeReportingDelay) # * STEP # Multiplicando pelo step para ajustar o calculo.
    
    # Variavel com DELAY - A definição das constantes aqui devem ser alteradas se as condicoes iniciais do modelo mudarem
    # Esta implementacao considera que os delays sempre serao iguais. Se os delays nao forem iguais, deve-se encontrar outra forma de implementar os delays (talvez com a equacao multiplicativa 1*(time > tempodelay)
    if(time > aTimeForHistoricalVolume) {
      nlinhas_delay = aTimeForHistoricalVolume / STEP
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[linha - nlinhas_delay,]
    } else {
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[1,]
    }
    
    aExpGrowthInVolume =  log(sReportedIndustryVolume/aLaggedIndustryVolume)/aTimeForHistoricalVolume
    
    aExpectedIndustryDemand = sReportedIndustryVolume*exp(aForecastHorizon*aCapacityAcquisitionDelay*aExpGrowthInVolume)
    
    list.variaveis.globais$aExpectedIndustryDemand[linha,] <<- aExpectedIndustryDemand
    
    # Mais uma variável com delay
    if(time > aCapacityAcquisitionDelay) {
      nlinhas_delay = aCapacityAcquisitionDelay / STEP
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[linha-nlinhas_delay,]
    } else {
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[1,]
    }
    
    aForecastError = (aLaggedVolumeForecast - aIndustryVolume)/(1e-009+aIndustryVolume)
    
    checkLaggedVolumeForecast = mean(aLaggedVolumeForecast)
    
    ##### TARGET CAPACITY SECTOR #####
    
    aIndustryCapacity = sum(aCapacity)
    
    aCompetitorCapacity = aIndustryCapacity - aCapacity
    
    aExpectedCompCapacity = aNormalCapacityUtilization*(aWeightOnSupplyLine*sPerceivedCompTargetCapacity+(1-aWeightOnSupplyLine)*aCompetitorCapacity)
    
    aUncontestedDemand = pmax(0, aExpectedIndustryDemand - aExpectedCompCapacity)
    
    aUncontestedMarketShare = aUncontestedDemand / aExpectedIndustryDemand
    
    aSwitchForCapacityStrategy1 = ifelse(aSwitchForCapacityStrategy == 1, 1, 0)
    aSwitchForCapacityStrategy2 = ifelse(aSwitchForCapacityStrategy == 2, 1, 0)
    aSwitchForCapacityStrategy3 = ifelse(aSwitchForCapacityStrategy == 3, 1, 0)
    aSwitchForCapacityStrategy4 = ifelse(aSwitchForCapacityStrategy == 4, 1, 0)
    
    aTargetMarketShare = {
        aSwitchForCapacityStrategy1*pmax(aDesiredMarketShare,aUncontestedMarketShare) +
        aSwitchForCapacityStrategy2*pmin(aDesiredMarketShare,aUncontestedMarketShare) +
        aSwitchForCapacityStrategy3*aDesiredMarketShare +
        aSwitchForCapacityStrategy4*aUncontestedMarketShare
    }
    
    
    aTargetCapacity = pmax(aMinimumEfficientScale,
                           aTargetMarketShare*aExpectedIndustryDemand/aNormalCapacityUtilization)
    
    aTargetNormalProduction = aTargetCapacity * aNormalCapacityUtilization
    
    aIndustryTotalTargetCapacity = sum(aTargetCapacity)
    
    aCompetitorTargetCapacity = aIndustryTotalTargetCapacity - aTargetCapacity
    
    fChangePerceivedCompTargetCapacity = (aCompetitorTargetCapacity - sPerceivedCompTargetCapacity) / aTimeToPerceiveCompTargetCapacity
    
    checkCompetitorTargetCapacity = mean(aCompetitorTargetCapacity)
    
    ##### CAPACITY SECTOR  - PT 2 - FLUXOS #####
    fchangeSmoothCapacity1 = (aTargetCapacity - sSmoothCapacity1) / (aCapacityAcquisitionDelay / 3)
    fchangeSmoothCapacity2 = (sSmoothCapacity1 - sSmoothCapacity2) / (aCapacityAcquisitionDelay / 3)
    fchangeSmoothCapacity3 = (sSmoothCapacity2 - sSmoothCapacity3) / (aCapacityAcquisitionDelay / 3)
    
    
    ##### LEARNING CURVE SECTOR #####
    fProduction = fShipments
    
    aLCExponent = log(aLCStrength)/log(2)
    
    aLearning = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent
    
    aInitialUnitFixedCost = (aInitialPrice/(1+aNormalProfitMargin))*aRatioOfFixedToVarCost*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aInitialUnitVariableCost = (aInitialPrice/(1+aNormalProfitMargin))*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aUnitFixedCost = aLearning * aInitialUnitFixedCost
    
    aUnitVariableCost = aLearning * aInitialUnitVariableCost
    
    checkUnitFixedCost = mean(aUnitFixedCost)
    
    checkUnitVariableCost = mean(aUnitVariableCost)
    
    ##### PRICE SECTOR #####
    
    aBasePrice = (1+aNormalProfitMargin)*(aUnitVariableCost+aUnitFixedCost/aNormalCapacityUtilization)
    
    aDemandSupplyBalance = aDesiredShipments/(aNormalCapacityUtilization*aCapacity)
    
    aTargetPrice = 
      pmax(aUnitVariableCost,
          sPrice*
            (1+aSensOfPriceToCosts*((aBasePrice/sPrice)-1))*
            (1+aSensOfPriceToDSBalance*(aDemandSupplyBalance-1))*
            (1+aSensOfPriceToShare*((aTargetMarketShare-aMarketShare))))
    
    checkTargetPrice = mean(aTargetPrice)
    
    fChangeInPrice = (aTargetPrice - sPrice) / aPriceAdjustmentTime
    
    ##### NET INCOME SECTOR #####
    
    aDiscountFactor = exp(-aDiscountRate*time) # 
    
    fValueOfNewOrders = fOrders * sPrice
    
    checkValueOfNewOrders1 = fValueOfNewOrders[1] #
    
    aAveragePriceOfOrderBook = sValueOfBacklog / sBacklog
    
    fRevenue = fShipments * aAveragePriceOfOrderBook #
    
    checkRevenue1 = fRevenue[1] #
    
    aVariableCost = fShipments * aUnitVariableCost #
    
    aFixedCost = aCapacity * aUnitFixedCost #
    
    fCost = aFixedCost + aVariableCost #
    
    fNetIncome = fRevenue - fCost #
    
    fNPVProfitChange = fNetIncome * aDiscountFactor #
    
    checkNPVProfitChange = mean(fNPVProfitChange) #
    
    aNPVIndustryProfits = sum(sNPVProfit) #
    
    
    ##### ESTOQUES - INICIAIS #####
    
    stocks_ini = list(
      BacklogIni = (1/length(fNetIncome)) * fIndustryOrderRate * aNormalDeliveryDelay
    )
    
    
    ##### ESTOQUES #####
    
    d_NPVProfit_dt = fNPVProfitChange
    
    d_ValueOfBacklog_dt = fValueOfNewOrders - fRevenue
    
    d_Backlog_dt = fOrders - fShipments
    
    d_InstalledBase_dt = fShipments - fDiscardRate
    
    d_Price_dt = fChangeInPrice
    
    d_CumulativeAdopters_dt = fAdoptionRate
    
    d_sReportedIndustryVolume_dt = fsmooth_ReportedIndustryVolume
    
    d_CumulativeProduction_dt = fProduction
    
    d_PerceivedCompTargetCapacity_dt = fChangePerceivedCompTargetCapacity
    
    d_SmoothCapacity1_dt = fchangeSmoothCapacity1
    
    d_SmoothCapacity2_dt = fchangeSmoothCapacity2
    
    d_SmoothCapacity3_dt = fchangeSmoothCapacity3
    
    ##### COMPARAR RESULTADOS COM O ITHINK #####
    
    if(VERIFICAR_STOCKS){
      for (variavel in variaveis_ithink_stocks) {
        # Definir o tipo de variavel
        # Variavel é um estoque?
        variavel_ithink_alterada = gsub(pattern = "\\[", replacement = "", x = variavel, ignore.case = TRUE)
        variavel_ithink_alterada = gsub(pattern = "\\]", replacement = "", x = variavel_ithink_alterada, ignore.case = TRUE)
        
        # Verificar apenas Estoques:
        variavel_ithink_alterada = paste("s", variavel_ithink_alterada, sep = "")
        
        # Valor da Variavel Calculada
        valor_variavel_R = eval(parse(text = variavel_ithink_alterada))
        
        valor_variavel_ithink = dados_ithink_stocks[[linha,variavel]]
        
        diferenca = valor_variavel_R - valor_variavel_ithink
        
        if (abs(x = diferenca) > CHECK_PRECISION){
          message(paste("Estoque Diff:", time, linha, variavel, diferenca, sep = " - "))
          if(BROWSE_ON_DIFF){
            browser()  
          }
        }
      }  
    }
    
    
    if(VERIFICAR_CHECKS){
      for (variavel in variaveis_ithink_checks) {
        # Definir o tipo de variavel
        # Variavel é um estoque?
        variavel_ithink_alterada = gsub(pattern = "\\[", replacement = "", x = variavel, ignore.case = TRUE)
        variavel_ithink_alterada = gsub(pattern = "\\]", replacement = "", x = variavel_ithink_alterada, ignore.case = TRUE)
        
        # Verificar apenas Estoques:
        #variavel_ithink_alterada = paste("s", variavel_ithink_alterada, sep = "")
        
        # Valor da Variavel Calculada
        valor_variavel_R = eval(parse(text = variavel_ithink_alterada))
        
        valor_variavel_ithink = dados_ithink_checks[[linha,variavel]]
        
        diferenca = valor_variavel_R - valor_variavel_ithink
        
        if(!is.na(diferenca)){
          if (abs(x = diferenca) > CHECK_PRECISION){
            message(paste("Check Diff:", time, linha, variavel, diferenca, sep = " - "))
            if(BROWSE_ON_DIFF){
              browser()  
            }
          }  
        }
        
      }
    }
    
    ##### VARIÁVEIS RETORNADAS #####
    
    ## Parar se o tempo chegou ao fim.
    if(time == FINISH){
    # browser()
    }
    
    resultado_completo = list(c(
      d_NPVProfit_dt
      ,d_ValueOfBacklog_dt
      ,d_Backlog_dt
      ,d_InstalledBase_dt
      ,d_Price_dt
      ,d_CumulativeAdopters_dt
      ,d_sReportedIndustryVolume_dt
      ,d_CumulativeProduction_dt
      ,d_PerceivedCompTargetCapacity_dt
      ,d_SmoothCapacity1_dt
      ,d_SmoothCapacity2_dt
      ,d_SmoothCapacity3_dt
    )
    ,fIndustryOrderRate = fIndustryOrderRate
    ,aNonAdopters = aNonAdopters
    ,fReorderRate = fReorderRate
    ,aIndustryShipments = aIndustryShipments
    ,aIndustryVolume = aIndustryVolume
    ,fDiscardRate = fDiscardRate
    ,aDiscountFactor = aDiscountFactor
    ,aDiscountRate = aDiscountRate
    ,fNPVProfitChange = fNPVProfitChange
    ,fNetIncome = fNetIncome
    ,aNPVIndustryProfits = aNPVIndustryProfits
    ,aInitialDemandForecast = aInitialDemandForecast
    ,aLaggedVolumeForecast = aLaggedVolumeForecast
    ,aForecastError = aForecastError
    ,aTargetCapacity = aTargetCapacity
    ,aCompetitorTargetCapacity = aCompetitorTargetCapacity)
    
    return (if(modo == "inicial"){
      stocks_ini
    } else {
      resultado_completo
    })   
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