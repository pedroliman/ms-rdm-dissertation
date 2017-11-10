library(deSolve)
library(ggplot2)
library(gdata)
library(scales)
library(FME)
library(readxl)

world_data <- read_xlsx(path = "D:/dev/SDMR/models/07Chapter/R/WorldPopulation.xlsx", sheet = "Sheet1")

world_data <- as.data.frame(world_data)

# world_data <- read.xls("D:/dev/SDMR/models/07Chapter/R/WorldPopulation.xlsx",stringsAsFactors=F)

START<-1960; FINISH<-2010; STEP<-0.1
simtime <- seq(START, FINISH, by=STEP)

WP_INIT<-world_data[1,"Population"]

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{  
    
    fPopulationAdded<-Population * aGrowthFraction
    
    fPopulationRemoved <- Population * aDeclineFraction
    
    dP_dt  <- fPopulationAdded - fPopulationRemoved
    
    return (list(c(dP_dt)))   
  })
}

solveWP <- function(pars){
  # All the stocks are initialised here...
  
  stocks  <- c(Population=WP_INIT)
  auxs    <- c(aGrowthFraction=unname(pars["aGrowthFraction"]),
               aDeclineFraction=unname(pars["aDeclineFraction"]))
  
  return (data.frame(ode(y=stocks, simtime, func = model, 
                         parms=auxs, method="euler")))
}

getCost<-function(p)
{
  out<-solveWP(p)
  #http://www.inside-r.org/packages/cran/FME/docs/modCost
  cost <- modCost(obs=world_data,model=out)
  #cat(str(cost))
  return(cost)
}

pars<-c(aGrowthFraction=0.1, aDeclineFraction=0.1) 
lower<-c(0.0, 0)
upper<-c(0.2, 0.3)


Fit<-modFit(p=pars,f=getCost,lower=lower,upper=upper)

optPar<-c(Fit$par)

cost = getCost(optPar)

optMod <- solveWP(optPar)

# see http://www.inside-r.org/packages/cran/FME/docs/modFit

time_points<-seq(from=1, to=length(simtime),by=1/STEP)
optMod<-optMod[time_points,]


Fit$ssr

p1<-ggplot()+geom_point(data=world_data,size=1.5,aes(time,Population,colour="Data"))+
  geom_line(data=optMod,size=1,aes(x=time,y=Population,colour="Model"))+
  ylab("People")+
  xlab("Year")+
  scale_y_continuous(labels = comma)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Data",
                               "Model"))
p1



