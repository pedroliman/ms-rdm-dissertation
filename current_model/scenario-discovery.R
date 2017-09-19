

library(sdtoolkit)

### ===== SDPRIM: =====

#Users confident in the soundness and appropriate formatting of their data may 
#take the following more direct actions: 


# Rodar Análise Anterior
source("modelo-difusao.R")

dados_finais = dplyr::filter(dados_simulacao, Tempo == FINISH)

#LOAD the data, either via:
mydata <- dados_finais
  

#Then define their input variables:
vetor_inputs = c("AdvEffectiveness", "ContactRate", "AdoptionFraction", "TotalPopulation",  "Adoption_Rate")

xmatrix <- mydata[,vetor_inputs]

xmatrix = as.matrix(xmatrix)
  
#Then define their output variable using EITHER
vetor_outputs = c("Adoption_From_Word_of_Mouth")
outputvar <- mydata[,vetor_outputs]
  
#OR
# outputvar <- mydata[,"outputname"]


  
#If output var is already a 0-1 variable, then sdprim can be called as:
# myboxes <- sdprim(x=xmatrix, y=outputvar)

  
#Otherwise, first threshold the output variable as follows:
hist(outputvar)

threshold = -80
outthresh <- 1*(outputvar>threshold)
  
#Then call sdprim:
myboxes <- sdprim(x=xmatrix, y=outthresh)




### ===== SEQ.INFO: =====

#To see a summary of sdprim output, 
data(exboxes)  #an example box sequence object included with the package
boxinfo <- seq.info(exboxes)


### ===== DIMPLOT: =====

#To see a 'Normalized Dimension Restriction Plot' for box i, type:
data(exboxes)
dimplot(exboxes, 1)
