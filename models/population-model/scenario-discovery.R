# A biblioteca plotly entra em conflito com uma função usada pelo OpenMORDM!
library(OpenMORDM)
assign("mordm.globals", new.env())
# Tentando fazer scenario discovery
# assign("mordm.globals", new.env(), envir=parent.env(environment()))

source("modelo-populacao.R")
dados_sd = dplyr::filter(dados_simulacao, Tempo == FINISH) 

# Variáveis a considerar na análise
vfatores = c("TaxaNascimento", "TaxaMorte", "Nascimentos", "Mortes")
factors = dados_sd[,vfatores]

vresposta = c("Populacao")
response = dados_sd[,vresposta]
# response = (dados_sd[,2] > 15000) * 1
thr = 15000
thr.type = 1 # >= x

# Esta linha de código agora funciona:
analyze.prim(factors, response, threshold = thr, threshold.type = thr.type, which.box = 1)

# Esta linha tem o CART, mas ainda não sei usar.
analise_cart = analyze.cart(factors, response)