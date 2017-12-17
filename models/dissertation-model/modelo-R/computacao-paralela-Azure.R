library(doAzureParallel)
### Configuração ####
# 1. Generate your credential and cluster configuration files.  
generateClusterConfig("cluster.json")
generateCredentialsConfig("credentials.json")

# 2. Fill out your credential config and cluster config files.
# Enter your Azure Batch Account & Azure Storage keys/account-info into your credential config ("credentials.json") and configure your cluster in your cluster config ("cluster.json")

# 3. Set your credentials - you need to give the R session your credentials to interact with Azure
setCredentials("credentials.json")

# 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
cluster <- makeCluster("cluster.json")

# 5. Register the pool as your parallel backend
registerDoAzureParallel(cluster)

# 6. Check that your parallel backend has been registered
getDoParWorkers()

# GErando resultado
number_of_iterations <- 10

func = function(n) n * 3
  
foreach(n = 1:10, .combine = c) %do% func(n)
  

results <- foreach(i = 1:number_of_iterations) %dopar% {
  # This code is executed, in parallel, across your cluster.
  i^2
}
