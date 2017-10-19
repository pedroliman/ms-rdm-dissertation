

library(sdtoolkit)

library(dplyr)


# Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

## Carregando o Modelo, e outros objetos
source('modelo-difusao.R', encoding = 'UTF-8')

opcoes = list(
  VarResposta = "Cash",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 100,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

results = simularRDM_e_escolher_estrategia(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes)

### ===== SDPRIM: =====

#Users confident in the soundness and appropriate formatting of their data may 
#take the following more direct actions: 

# Rodar Análise Anterior
# source("modelo-difusao.R")

dados = results$AnaliseRegret$Dados

estrategia = results$EstrategiaCandidata

vetor_outputs = c("CashRegretPerc")
vetor_inputs = c("AdvEffectiveness", "ContactRate", "AdoptionFraction", "AverageTicket",  "AdvertisingCost")
threshold = 0.4

dados_estrategia = dplyr::filter(dados, Lever == estrategia)

#LOAD the data, either via:
mydata <- dados_estrategia
  
#Then define their input variables:

xmatrix <- mydata[,vetor_inputs]

xmatrix = as.matrix(xmatrix)
  
#Then define their output variable using EITHER
outputvar <- mydata[,vetor_outputs]
  
#OR
# outputvar <- mydata[,"outputname"]


  
#If output var is already a 0-1 variable, then sdprim can be called as:
# myboxes <- sdprim(x=xmatrix, y=outputvar)

  
#Otherwise, first threshold the output variable as follows:
# hist(outputvar)
outthresh <- 1*(outputvar>threshold)
  
#Then call sdprim:
myboxes <- sdprim(x=xmatrix, y=outthresh)


# Testando a biblioteca prim, pura:
prim_box = prim::prim.box(x = xmatrix, y = outputvar, threshold = threshold, threshold.type = 1)









dimplot(myboxes, 1)

### ===== SEQ.INFO: =====

#To see a summary of sdprim output, 
data(exboxes)  #an example box sequence object included with the package
boxinfo <- seq.info(exboxes)
library(prim)



### ===== DIMPLOT: =====

#To see a 'Normalized Dimension Restriction Plot' for box i, type:
data(exboxes)
dimplot(exboxes, 1)

dimplot(myboxes, 1)

