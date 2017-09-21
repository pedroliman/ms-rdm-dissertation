library(OpenMORDM)
library(lhs)



# Load the dataset
load("D:/dev/ms-rdm-dissertation/outros/openmordm/precomputed.RData")


# Show the designs in a 3D scatter plot and parallel coordinates plot
palette <- colorRampPalette(c("green", "yellow", "red"))(100)
mordm.plot(data, color="Reliability", palette=palette)
mordm.plot.parallel()

# Generate the 3D scatter plot figures of the 4 robustness measures
mordm.plot(data, color=robustness[,"Regret Type I"])
mordm.plot(data, color=robustness[,"Regret Type II"])
mordm.plot(data, color=robustness[,"Satisficing Type I"])
mordm.plot(data, color=robustness[,"Satisficing Type II"], clim=c(-1, 0))

# Scenario discovery
design_id <- 274
analyze.prim(factors, samples[[design_id]]$objs[,"Reliability"], threshold.type=-1)
analyze.cart(factors, ifelse(samples[[design_id]]$objs[,"Reliability"]<1, "Failure", "Success"))

# Display results with web visualization toolkit
explore(mordm.get.set(data), nobjs=4)
