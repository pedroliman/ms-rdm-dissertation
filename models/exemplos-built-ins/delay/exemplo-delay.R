library(deSolve)

# Para usar o delay de tempo no R, ? necess?rio usar a funcao lagvalue, que requer outros "solvers" para as equa??es diferenciais, exigindo outros m?todos de integra??o, que n?o ? o m?todo Euler.
# Isso faz com que os resultados n?o fechem "na virgula" com o Ithink, quando usa-se o m?todo Runge-Kutta.


# Neste arquivo apenas ficarÃ¡ o modelo de dinÃ¢mica de sistemas.
# Definindo Tempos da SimulaÃ§Ã£o
START<-0; FINISH<-20; STEP<-0.25

# Vetor de Tempos
tempo <- seq(START, FINISH, by=STEP)

# Entrada
parametros = c(inputx = 0.1, timedelay = 2)

# Estoques
estoques = c(stock = 10)

estoque_inicial = 0

# Modelo

modelo = function(tempo, parametros, estoques){
  with(as.list(c(parametros, estoques)),{
    
    # Fluxo a calcular - Mudanca do Output - Output Exponencial de primeira orde
    flow = inputx * stock
    
    
    # Vari?vel com Delay pode observar um resultado apenas dos estoques, ou de um fluxo adicionado a estoque
    if(tempo > timedelay) {
      delayedvariable = lagvalue(tempo - timedelay)[1]
    } else {
      delayedvariable = 10
    }
    
    #Estoque a calcular
    stock = flow
    
    # Resultado
    return(list(c(stock)
                ,delayedvariable = delayedvariable
                ))
    
  })
}

# Rodando o modelo no R
resultados = dede(y = estoques, times = tempo, func = modelo, parms = parametros)

