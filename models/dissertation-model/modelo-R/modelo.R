# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
START<-0; FINISH<-20; STEP<-0.25

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)

# Criando Estoques (na mão em um primeiro momento).
auxs    <- list(aDiscountRate = 0.04
                ,fNetIncome = c(10,10)
                )

# Variaveis definidas de modo independente (porque são vetorizadas)


# Os aux devem ser um vetor normal

# Número de Players no modelo
N_PLAYERS = 2

# A ORDEM AQUI DEVE SER A MESMA DA ORDEM DE SAÍDA DO MODELO!!!!!!!
stocks  <- c(NPVProfit=c(0,0))

##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    # Fluxos
    aDiscountFactor = exp(-aDiscountRate*time)
    
    fNPVProfitChange = fNetIncome * aDiscountFactor
    
    # Estoques
    d_NPVProfit_dt = fNPVProfitChange
    
    return (list(c(d_NPVProfit_dt)
                 ,aDiscountFactor = aDiscountFactor
                 ,aDiscountRate = aDiscountRate
                 ,fNPVProfitChange = fNPVProfitChange
                 ,fNetIncome = fNetIncome))   
  })
}


# Nomeando o Dataframe de Saída
nomes_variaveis = c("Tempo", "d_NPVProfit_dt", "aDiscountFactor", "aDiscountRate", "fNPVProfitChange", "fNetIncome")


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