## =============================================================================
## exercise 6 from Shampine and Thompson, 2000
## solving delay differential equations with dde23
##
## two lag values
## =============================================================================
##-----------------------------
## the derivative function
##-----------------------------
derivs <- function(t, y, parms) {
  History <- function(t) c(cos(t), sin(t))
  
  browser()
  
  if (t < 1)
    lag1 <- History(t - 1)[1]
  else
    lag1 <- lagvalue(t - 1)[1] # returns a vector; select first element
  if (t < 2)
    lag2 <- History(t - 2)[2]
  else
    lag2 <- lagvalue(t - 2,2) # faster than lagvalue(t - 2)[2]
  dy1 <- lag1 * lag2
  dy2 <- -y[1] * lag2
  list(c(dy1, dy2), lag1 = lag1, lag2 = lag2)
}
##-----------------------------
## parameters
##-----------------------------
r <- 3.5; m <- 19
##-----------------------------
## initial values and times
##-----------------------------
yinit <- c(y1 = 0, y2 = 0)
times <- seq(0, 20, by = 0.01)
##-----------------------------
## solve the model
##-----------------------------
yout <- dede(y = yinit, times = times, func = derivs,
             parms = NULL, atol = 1e-9)