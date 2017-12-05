library(ggplot2)

## Esta função gera um gráfico de curvas de experiências para demonsração da relação entre experiência acumulada e custos.

gerar_grafico_curva_experiencia = function() {
  aLCStrength = c(0.5, 0.75, 0.85, 0.95, 1)
  
  aLCExponent = log(aLCStrength)/log(2)
  
  aInitialProductionExperience = 1000
  
  sCumulativeProduction = 1000:5000
  
  learning_df = data.frame(Learning0.5 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[1],
                           Learning0.75 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[2],
                           Learning0.85 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[3],
                           Learning0.95 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[4],
                           Learning1 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[5])
  
  aInitialUnitFixedCost = 2000
  
  aInitialUnitVariableCost = 1000
  
  # aUnitFixedCost = aLearning * aInitialUnitFixedCost
  
  aUnitVariableCost = data.frame(learning_df * aInitialUnitVariableCost, Producao = sCumulativeProduction) 
  
  ggplot2::ggplot(aUnitVariableCost, aes(Producao)) + 
    geom_line(aes(y = Learning1, colour = "1")) + 
    geom_line(aes(y = Learning0.95, colour = "0.95")) + 
    geom_line(aes(y = Learning0.85, colour = "0.85")) + 
    geom_line(aes(y = Learning0.75, colour = "0.75")) + 
    geom_line(aes(y = Learning0.5, colour = "0.5")) + 
    ylab("Custo Variável de Produção") + 
    xlab("Produção Acumulada") +
    labs(color = expression(Gamma))
}