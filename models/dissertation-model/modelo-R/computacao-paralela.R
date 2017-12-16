library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

base = 2

# Initiate cluster
cl <- makeCluster(no_cores)

# Exportando objetos que preciso ter nos clusters:
clusterExport(cl, "obter_quadrado")

# Executando código no cluster:
#clusterEvalQ(cl, library(rms))

parSapply(cl, 2:4, 
          function(exponent) 
            base^exponent)

parLapply(cl, 2:4,
          function(exponent)
            2^exponent+base)

out <- parLapply(cl, linhas_ensemble, obter_quadrado)

stopCluster(cl)


linhas_ensemble = 1:100

obter_quadrado = function(numero){
  message(numero)
  numero^2
}




out <- lapply(linhas_ensemble, obter_quadrado)
do.call(rbind, out)

do.call(rbind, out)

