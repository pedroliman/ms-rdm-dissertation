# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
library(dplyr)

## Inicializar variaveis da simulacao aqui:
# START<-0; FINISH<-40; STEP<-0.0625

# VERIFICAR_STOCKS = FALSE

# VERIFICAR_CHECKS = TRUE

# CHECK_PRECISION = 0.001

#BROWSE_ON_DIFF = TRUE

# Vetor de Tempos
# SIM_TIME <- seq(START, FINISH, by=STEP)


##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs, modo = "completo"){
  with(as.list(c(stocks, auxs)),{
    # Criando uma variavel n_tempo local
    n_tempo = nrow(list.variaveis.globais$sReportedIndustryVolume)
    
    # if(modo == "inicial") {
    #   browser()
    # }

    
    ##### VETORIZANDO ESTOQUES #####
    
    #Estoques Vetorizados = substituindo estoques pela forma vetorizada (pra que seja possivel formular equações de forma mais simples).
    # Esta implementação tem por objetivo não gerar a necessidade de referenciar os estoque spelo seu nome único
    sNPVProfit = stocks[grep("sNPVProfit", x = names(stocks))]
    sValueOfBacklog = stocks[grep("sValueOfBacklog", x = names(stocks))]
    sBacklog = stocks[grep("sBacklog", x = names(stocks))]
    sInstalledBase = stocks[grep("sInstalledBase", x = names(stocks))]
    sPrice = stocks[grep("sPrice", x = names(stocks))]
    sCumulativeAdopters = stocks[grep("sCumulativeAdopters", x = names(stocks))]
    sReportedIndustryVolume = stocks[grep("sReportedIndustryVolume", x = names(stocks))]
    sCumulativeProduction = stocks[grep("sCumulativeProduction", x = names(stocks))]
    sPerceivedCompTargetCapacity = stocks[grep("sPerceivedCompTargetCapacity", x = names(stocks))]
    sSmoothCapacity1 = stocks[grep("sSmoothCapacity1", x = names(stocks))]
    sSmoothCapacity2 = stocks[grep("sSmoothCapacity2", x = names(stocks))]
    sSmoothCapacity3 = stocks[grep("sSmoothCapacity3", x = names(stocks))]
    
    sInvestimentoNaoRealizadoPeD = stocks[grep("sInvestimentoNaoRealizadoPeD", x = names(stocks))]
    sPatentesRequisitadas = stocks[grep("sPatentesRequisitadas", x = names(stocks))]
    sPatentesEmpresa = stocks[grep("sPatentesEmpresa", x = names(stocks))]
    sPatentesEmDominioPublicoUteis = stocks[grep("sPatentesEmDominioPublicoUteis", x = names(stocks))]
    sInvestimentoPeDDepreciar = stocks[grep("sInvestimentoPeDDepreciar", x = names(stocks))]
    
    #Obtendo o número da linha no qual estou
    linha = ((START - time) * (n_tempo - 1)) / FINISH + 1
    
    # Gravando a Variável sReportedIndustryVolume no vetor global
    list.variaveis.globais$sReportedIndustryVolume[linha,] <<- sReportedIndustryVolume
    
    
    ### Calculando Variáveis para o Estoque Inicial de Cumulative Adopters
    
    aEstimatedAdopters = aTotalInitialInstalledBase / aUnitsPerHousehold
    
    aInitialNewAdoptersOrderRate = aInitialIndustryShipments*(1-aInitialReorderShare)
    
    aInitialAdoptionRate = aInitialNewAdoptersOrderRate / aUnitsPerHousehold
    
    aInitialCumulativeAdopters2 = ((aInitialAdoptionRate/(aPopulation-aEstimatedAdopters))-aInnovatorAdoptionFraction)*(aPopulation/aWOMStrength)
    
    aInitialCumulativeAdopters = aInitialCumulativeAdopters2
    
    #browser()
    
    ##### DIFFUSION SECTOR #####
    aDemandCurveSlope = - aReferenceIndustryDemandElasticity * (aReferencePopulation / aReferencePrice )
    
    aLowestPrice = min(sPrice)
    
    aIndustryDemand = min(
      aPopulation,
      aReferencePopulation * max(
        0,
        1 + aDemandCurveSlope * (aLowestPrice - aReferencePrice) / aReferencePopulation
      )
    )
    
    checkIndustryDemand = aIndustryDemand
    
    # A fórmula abaixo não é mais utilizada.
    # aInitialCumulativeAdopters = aInitialDiffusionFraction * aIndustryDemand
    
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
    
    # Patentes e Performance
    aPatentesEmpresaTemAcesso = sPatentesRequisitadas + sPatentesEmpresa + sPatentesEmDominioPublicoUteis
    
    aPerformanceCalculada = aPerfSlope * aPatentesEmpresaTemAcesso
    
    aPerformance = pmax(aPerfMin, pmin(aPerfMax, aPerformanceCalculada))
    
    checkPerformance = mean(aPerformance)
    
    aAttractivenessFromPerformance = aPeDLigado * exp(aSensOfAttractToPerformance*(aReferencePerformance/aPerformance)) + (1 - aPeDLigado)
    
    aAttractivenessFromAvailability = exp(aSensOfAttractToAvailability*(aDeliveryDelay/aReferenceDeliveryDelay))
    
    aAttractivenessFromPrice = exp(aSensOfAttractToPrice*(sPrice/aReferencePrice))
    
    aAttractiveness = aAttractivenessFromAvailability * aAttractivenessFromPrice * aAttractivenessFromPerformance
    
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
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[(linha - nlinhas_delay),]
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
    
    
    
    ##### Custo P e D ####
    aTempoDepreciacao = aTempoMedioAvaliacao + aTempoVencimentoPatentes + aTempoMedioRealizacaoPeD
    
    fDepreciacaoInvPeD = sInvestimentoPeDDepreciar / aTempoDepreciacao
    
    aPeDUnitCost = fDepreciacaoInvPeD / fShipments
    
    
    
    
    ##### LEARNING CURVE SECTOR #####
    fProduction = fShipments
    
    aLCExponent = log(aLCStrength)/log(2)
    
    aLearning = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent
    
    aInitialUnitFixedCost = (aInitialPrice/(1+aNormalProfitMargin))*aRatioOfFixedToVarCost*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aInitialUnitVariableCost = (aInitialPrice/(1+aNormalProfitMargin))*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aUnitFixedCost = aLearning * aInitialUnitFixedCost + aPeDUnitCost * aPeDLigado
    
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
    
    ##### P&D - Investimento #####
    
    fInvestimentoPeD = fRevenue * aOrcamentoPeD * aPeDLigado
    
    fInvestimentoPeDRealizado = sInvestimentoNaoRealizadoPeD / aTempoMedioRealizacaoPeD
    
    fPatentesSolicitadas = fInvestimentoPeDRealizado / aCustoMedioPatente
    
    fPatentesRejeitadas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * aTaxaRejeicao
    
    fPatentesConcedidas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * (1-aTaxaRejeicao)
    
    fPatentesVencidas = sPatentesEmpresa / aTempoVencimentoPatentes
    
    fPatentesUtilidadeExpirada = sPatentesEmDominioPublicoUteis / aTempodeInutilizacaoPatente
    
    ##### NET INCOME - PARTE 2 #####
    
    checkRevenue1 = fRevenue[1] #
    
    aVariableCost = fShipments * aUnitVariableCost #
    
    aFixedCost = aCapacity * (aUnitFixedCost - (aPeDUnitCost * aPeDLigado)) #
    
    fCost = aFixedCost + aVariableCost #
    
    fNetIncome = fRevenue - fCost - fInvestimentoPeD #
    
    fNPVProfitChange = fNetIncome * aDiscountFactor #
    
    checkNPVProfitChange = mean(fNPVProfitChange) #
    
    checkNPVProfitChange1 = fNPVProfitChange[1]
    
    
    aNPVIndustryProfits = sum(sNPVProfit) #
    
    
   
    
    

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
    
    #Estoques do Investimento em PeD
    
    d_InvestimentoNaoRealizadoPeD_dt = fInvestimentoPeD - fInvestimentoPeDRealizado
    
    d_PatentesRequisitadas_dt = fPatentesSolicitadas - fPatentesConcedidas - fPatentesRejeitadas
    
    d_PatentesEmpresa_dt = fPatentesConcedidas - fPatentesVencidas
    
    d_PatentesEmDominioPublicoUteis_dt = sum(fPatentesVencidas) - fPatentesUtilidadeExpirada
    
    d_InvestimentoPeDDepreciar_dt = fInvestimentoPeD - fDepreciacaoInvPeD
    
    
    
    # Variaveis de Estoques Iniciais
    
    BacklogIni = aInitialSharePlayers * fIndustryOrderRate * aNormalDeliveryDelay
    
    InstalledBaseIni = aInitialCumulativeAdopters * aInitialSharePlayers * aUnitsPerHousehold
    
    CumulativeAdoptersIni = aInitialCumulativeAdopters
    
    ValueOfBacklogIni = aInitialSharePlayers * fIndustryOrderRate * aNormalDeliveryDelay * aInitialPrice
    
    ReportedIndustryVolumeIni = aIndustryVolume
    
    CumulativeProductionIni = aInitialProductionExperience
    
    PerceivedCompTargetCapacityIni = aCompetitorCapacity
    
    CapacityIni = aInitialSharePlayers * fIndustryOrderRate / aNormalCapacityUtilization
    
    InitialInvestimentoNaoRealizadoPeD = aInitialInvestimentoNaoRealizadoPeD * aPatentShare
    
    InitialPatentesRequisitadas = aInitialPatentesRequisitadas * aPatentShare
    
    InitialPatentesEmpresa = aInitialPatentesEmpresa * aPatentShare
    
    InitialsPatentesEmDominioPublicoUteis =  aInitialsPatentesEmDominioPublicoUteis
    
    InitialsInvestimentoPeDDepreciar = aInitialsInvestimentoPeDDepreciar * aPatentShare
    

    ##### ESTOQUES - INICIAIS #####
    
    stocks_ini = list(
      BacklogIni = BacklogIni,
      InstalledBaseIni = InstalledBaseIni,
      CumulativeAdoptersIni = CumulativeAdoptersIni,
      ValueOfBacklogIni = ValueOfBacklogIni,
      ReportedIndustryVolumeIni = ReportedIndustryVolumeIni,
      CumulativeProductionIni = CumulativeProductionIni,
      PerceivedCompTargetCapacityIni = PerceivedCompTargetCapacityIni,
      CapacityIni = CapacityIni,
      
      InitialInvestimentoNaoRealizadoPeD = InitialInvestimentoNaoRealizadoPeD,
      InitialPatentesRequisitadas = InitialPatentesRequisitadas,
      InitialPatentesEmpresa = InitialPatentesEmpresa,
      InitialsPatentesEmDominioPublicoUteis = InitialsPatentesEmDominioPublicoUteis,
      InitialsInvestimentoPeDDepreciar = InitialsInvestimentoPeDDepreciar
    )
    
    ##### COMPARAR RESULTADOS COM O ITHINK #####
    
    if(VERIFICAR_STOCKS & modo == "completo"){
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
    
    
    if(VERIFICAR_CHECKS & modo == "completo"){
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
    
    # Colocar isso dentro do IF abaixo e verificar!
    if(VERIFICAR_GLOBAL & modo == "completo"){
      
      # Forma que usa todas as variaveis do ambiente:
      # variaveis_disponiveis_ambiente = ls()
      # variaveis_auxiliares = variaveis_disponiveis_ambiente[grep("^[aA].*", variaveis_disponiveis_ambiente)]
      # 
      # Forma que usa as variaveis globais definidas em um vetor
      variaveis_auxiliares = variaveis_globais_a_verificar
      
      
      seletor_players = paste("[",1:N_PLAYERS,"]", sep = "")
      
      variaveis_a_verificar = expand.grid(variaveis_auxiliares, seletor_players)
      
      variaveis_a_verificar = paste(variaveis_a_verificar[,1], variaveis_a_verificar[,2], sep = "")
      
      variaveis_a_verificar_no_ithink = substring(variaveis_a_verificar, 2)
      
      
      verificar_variaveis_globais = function(n_variavel){
        valor_variavel_R = eval(parse(text = variaveis_a_verificar[n_variavel]))
        
        valor_variavel_ithink = dados_ithink_global[[linha,variaveis_a_verificar_no_ithink[n_variavel]]]
        
        if((length(valor_variavel_ithink) > 0)) {
          decisao = !is.na(valor_variavel_ithink) & is.numeric(valor_variavel_ithink)
          if(decisao == FALSE) {
            valor_variavel_ithink = NA
          }
        } else {valor_variavel_ithink = NA}
        
        
        if((length(valor_variavel_R) > 0)) {
          decisao = !is.na(valor_variavel_R) & is.numeric(valor_variavel_R)
          if(decisao == FALSE) {
            valor_variavel_R = NA
          }
        } else {valor_variavel_R = NA}
        
        
        diferenca = unname(valor_variavel_R)  - unname(valor_variavel_ithink)
        
        diferenca
      }
      
      diferencas = lapply(X = 1:length(variaveis_a_verificar), FUN = verificar_variaveis_globais)
      
      diferencas = do.call(rbind, diferencas)
      
      matriz_diferencas = data.frame(
        variaveis_a_verificar,
        diferencas
      )
      
      diferencas_a_reportar = subset(matriz_diferencas, abs(diferencas) > CHECK_PRECISION)
      
      if(nrow(diferencas_a_reportar)>1) {
        message(paste("Check Diferenças Globais:", time, linha, sep = " - "))
        if(BROWSE_ON_DIFF){
          browser()  
        }
      }

    }
    
    ##### VARIÁVEIS RETORNADAS #####
    
    ## Parar se o tempo chegou ao fim.
    # if(time == FINISH){
    # browser()
    # }
    
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
      ,d_InvestimentoNaoRealizadoPeD_dt
      ,d_PatentesRequisitadas_dt
      ,d_PatentesEmpresa_dt
      ,d_PatentesEmDominioPublicoUteis_dt
      ,d_InvestimentoPeDDepreciar_dt
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

# Inicializando um list com Tudo o que é necessário para a Simulação.
sdmodel = list(
  Start = START,
  Finish = FINISH,
  Step = STEP,
  SimTime = SIM_TIME,
  # Auxs = auxs,
  # Stocks = stocks,
  Modelo = modelo,
  Variaveis = nomes_variaveis
)
