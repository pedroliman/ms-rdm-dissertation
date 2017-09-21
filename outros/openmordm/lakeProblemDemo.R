library(OpenMORDM)
library(lhs)

# Step 1. Generate and plot the Pareto approximate set
nvars <- 100
nobjs <- 4
nconstrs <- 1

problem <- define.problem("./lake.exe", nvars, nobjs, nconstrs,
	bounds=matrix(rep(range(0, 0.1), nvars), nrow=2),
	names=c("Phosphorus in Lake", "Economic Benefit", "Inertia", "Reliability"),
	maximize=c("Economic Benefit", "Inertia", "Reliability"),
	epsilons=c(0.0001,0.0001,0.000001,0.000001))
	
data <- borg.optimize(problem, 100000)
palette <- colorRampPalette(c("green", "yellow", "red"))(100)
mordm.plot(data, color="Reliability", palette=palette)
mordm.plot.parallel()

# Step 2. Setup the computational models for MORDM
nsamples <- 1000
param_names <- c("b", "q", "mean", "sigma", "delta")
param_bounds <- matrix(c(
	c(0.1, 0.45),
	c(2, 4.5),
	c(0.01, 0.05),
	c(0.001, 0.005),
	c(0.93, 0.99)), nrow=2)
baseline_factors <- c(0.42, 2, 0.02, 0.0017, 0.98)

factors <- randomLHS(nsamples, length(param_names))
factors <- t(apply(factors, 1, function(x) x*diff(param_bounds)+param_bounds[1,]))
colnames(factors) <- param_names

model_calls <- do.call(sprintf, c(list("./lake.exe -b %f -q %f -m %f -s %f -d %f"),
 								  lapply(1:ncol(factors), function(i) factors[,i])))

models <- lapply(model_calls, function(command) {
	define.problem(command, nvars, nobjs, nconstrs,
		  bounds=matrix(rep(range(0, 0.1), nvars), nrow=2),
		  names=c("Phosphorus in Lake", "Economic Benefit", "Inertia", "Reliability"),
		  maximize=c("Economic Benefit", "Inertia","Reliability"))
})

# Step 3. Evaluate each Pareto approximate solution under deep uncertainty
uncertainty.samples <- mordm.sample.uncertainties(data, nsamples, models)
robustness <- mordm.evaluate.uncertainties(uncertainty.samples,
		function(x) sum(abs(x$constrs)) == 0 && x$objs[2]>0.1,
		factors,
		baseline_factors)

mordm.plot(data, color=robustness[,"Regret Type I"])
mordm.plot(data, color=robustness[,"Regret Type II"])
mordm.plot(data, color=robustness[,"Satisficing Type I"])
mordm.plot(data, color=robustness[,"Satisficing Type II"], clim=c(-1, 0))

# Step 4. Scenario discovery
selected.point <- uncertainty.samples[[sample(1:length(uncertainty.samples), 1)]]
analyze.prim(factors, selected.point$objs[,"Reliability"], threshold.type=-1)
analyze.cart(factors, ifelse(selected.point$objs[,"Reliability"]<1, "Polluted", "Unpolluted"))

# Optional Step. Display results with web visualization toolkit
display.set <- mordm.cbind(mordm.get.set(data), robustness)
explore(display.set, nobjs=4+ncol(robustness))
