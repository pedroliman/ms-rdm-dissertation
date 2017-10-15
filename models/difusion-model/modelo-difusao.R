# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo Tempos da Simulação
START<-2015; FINISH<-2020; STEP<-0.125

# Vetor de Tempos
simtime <- seq(START, FINISH, by=STEP)

# Criando Estoques (na mão em um primeiro momento).
auxs    <- c(aAdvertisingEffectiveness= 0.01, aContactRate= 100, aAdoptionFraction= 0.02, aTotalPopulation= 1000000, aAdvertisingON = 1, aAdvertisingIntensity = 1, aAverageTicket = 1, aAdvertisingCost = 1)

# A ORDEM AQUI DEVE SER A MESMA DA ORDEM DE SAÍDA DO MODELO!!!!!!!
stocks  <- c(sPotentialAdopters=999990, sAdopters=10, sCash=0)

##### Modelo de Dinâmica de Sistemas ####

# Definindo o Modelo
modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    aAdoption_from_Advertising = aAdvertisingEffectiveness * sPotentialAdopters * aAdvertisingON * aAdvertisingIntensity
    
    aAdoption_from_Word_of_Mouth = aContactRate * sAdopters *  ((sPotentialAdopters)/(aTotalPopulation)) * aAdoptionFraction  # {people/year}
    
    fAdoption_Rate = min(aAdoption_from_Advertising + aAdoption_from_Word_of_Mouth, sPotentialAdopters) # {people/year}
    
    fRevenue = sAdopters * aAverageTicket
    
    fCosts = aAdvertisingIntensity * aAdvertisingCost
    
    d_sPotentialAdopters_dt = - fAdoption_Rate
    
    d_sAdopters_dt = fAdoption_Rate
    
    d_sCash_dt = fRevenue - fCosts
    
    return (list(c(d_sPotentialAdopters_dt, d_sAdopters_dt,  d_sCash_dt),
                 Revenue = fRevenue,
                 Costs = fCosts,
                 AverageTicket = aAverageTicket,
                 AdvertisingCost = aAdvertisingCost,
                 AdvertisingEffectiveness = aAdvertisingEffectiveness,
                 ContactRate = aContactRate,
                 AdoptionFraction = aAdoptionFraction,
                 TotalPopulation = aTotalPopulation,
                 Adoption_from_Advertising = aAdoption_from_Advertising,
                 Adoption_from_Word_of_Mouth = aAdoption_from_Word_of_Mouth,
                 Adoption_Rate = fAdoption_Rate,
                 AdvertisingON = aAdvertisingON,
                 AdvertisingIntensity = aAdvertisingIntensity,
                 Lever = Lever))   
  })
}

# Nomeando o Dataframe de Saída
nomes_variaveis_final = c("Tempo", "PotentialAdopters", "Adopters", "Cash", "Revenue", "Costs","AverageTicket","AdvertisingCost", "AdvEffectiveness", "ContactRate", "AdoptionFraction", "TotalPopulation", "Adoption_From_Advertising", "Adoption_From_Word_of_Mouth", "Adoption_Rate", "AdvON", "AdvIntensity", "Lever", "Scenario")

