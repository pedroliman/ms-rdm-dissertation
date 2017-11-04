library(deSolve)

# Neste arquivo apenas ficarÃ¡ o modelo de dinÃ¢mica de sistemas.
# Definindo Tempos da SimulaÃ§Ã£o
START<-0; FINISH<-20; STEP<-0.25

# Vetor de Tempos
tempo <- seq(START, FINISH, by=STEP)

# Entrada
parametros = c(input = 10, avgtime = 1)

# Estoques
estoques = c(sSaida = 0)

# Modelo

modelo = function(tempo, parametros, estoques){
  with(as.list(c(parametros, estoques)),{
    
    # Fluxo a calcular - Mudanca do Output - Output Exponencial de primeira orde
    fMudancaOutput = ((input - sSaida) / avgtime) * STEP # Multiplicando pelo step para ajustar o calculo.
    
    #Estoque a calcular
    d_sSaida_dt = fMudancaOutput
    
    
    # Resultado
    return(list(c(d_sSaida_dt),
                fMudancaOutput = fMudancaOutput
                ))
    
  })
}

# Rodando o modelo no R
resultados = ode(y = estoques, times = tempo, func = modelo, parms = parametros)

