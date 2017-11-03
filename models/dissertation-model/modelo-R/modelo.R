# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
START<-0; FINISH<-20; STEP<-1

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)

# Número de Players no modelo
N_PLAYERS = 2

# Criando Estoques (na mão em um primeiro momento).
auxs    <- list(aDiscountRate = 0.04
                ,fOrders = rep(1, times = N_PLAYERS)
                ,fShipments = rep(1, times = N_PLAYERS)
                ,fDiscardRate = rep(1, times = N_PLAYERS)
                ,fChangeInPrice = rep(0, times = N_PLAYERS)
                ,aUnitVariableCost = rep(10, times = N_PLAYERS)
                ,aUnitFixedCost = rep(10, times = N_PLAYERS)
                ,aCapacity = rep(1, times = N_PLAYERS)
                )

# A ORDEM AQUI DEVE SER A MESMA DA ORDEM DE SAÍDA DO MODELO.
stocks  <- c(
   sNPVProfit = rep(0, times = N_PLAYERS)
  ,sValueOfBacklog = rep(1, times = N_PLAYERS)
  ,sBacklog = rep(1, times = N_PLAYERS)
  ,sInstalledBase = rep(1, times = N_PLAYERS)
  ,sPrice = rep(30, times = N_PLAYERS)
             )

##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    #Estoques Vetorizados = substituindo estoques pela forma vetorizada (pra que seja possivel formular equações de forma mais simples).
    # Esta implementação tem por objetivo não gerar a necessidade de referenciar os estoque spelo seu nome único
    sNPVProfit = stocks[(N_PLAYERS*0+1):(N_PLAYERS*1)]
    sValueOfBacklog = stocks[(N_PLAYERS*1+1):(N_PLAYERS*2)]
    sBacklog = stocks[(N_PLAYERS*2+1):(N_PLAYERS*3)]
    sInstalledBase = stocks[(N_PLAYERS*3+1):(N_PLAYERS*4)]
    sPrice = stocks[(N_PLAYERS*4+1):(N_PLAYERS*5)]
    
    
    ##### NET INCOME SECTOR  - Fluxos e Auxiliares #####
    
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
    
    # Estoques
    #Profit
    d_NPVProfit_dt = fNPVProfitChange
    
    d_ValueOfBacklog_dt = fValueOfNewOrders - fRevenue
    
    d_Backlog_dt = fOrders - fShipments
    
    d_InstalledBase_dt = fShipments - fDiscardRate
    
    d_Price_dt = fChangeInPrice
    
    
    return (list(c(
                   d_NPVProfit_dt
                   ,d_ValueOfBacklog_dt
                   ,d_Backlog_dt
                   ,d_InstalledBase_dt
                   ,d_Price_dt
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