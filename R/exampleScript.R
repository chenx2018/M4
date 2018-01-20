### This is an example script of how to do stuff in a loop ###

library(Mcomp);
library(smooth);
library(forecast);

# Make a subset from M4
M4Subset <- subset(M4,"QUARTERLY");

# Substitute nSeries by the length of the desired subset.
# e.g. nSeries <- length(M4Subset)
nSeries <- 3;


#### Accuracy ####

# Produce errors for your method in a loop
errorsFromModel <- matrix(NA,nSeries,2,dimnames=list(NULL,c("SMAPE","MASE")));
for(i in 1:nSeries){
    testModel <- es(M4Subset[[i]]);
    errorsFromModel[i,] <- errorMeasurer(M4Subset[[i]]$xx,testModel$forecast,M4Subset[[i]]$x);
}

# After that - forecasts from naive
# The organisers use different stuff - they deseasonalise data and then apply Naive
# But that's too much work, which changes nothing in our case.
# So I use seasonal naive instead.
errorsFromNaive2 <- matrix(NA,nSeries,2,dimnames=list(NULL,c("SMAPE","MASE")));
for(i in 1:nSeries){
    testModel <- snaive(M4Subset[[i]]$x,h=M4Subset[[i]]$h);
    errorsFromNaive2[i,] <- errorMeasurer(M4Subset[[i]]$xx,testModel$mean,M4Subset[[i]]$x);
}

# Finally, calculate OWA:
OWA(errorsFromModel,errorsFromNaive2);



#### Intervals ####

MSISValues <- rep(NA,nSeries);

# Estimate a model and produce MSIS in a loop
for(i in 1:nSeries){
    testModel <- es(M4Subset[[i]], intervals=T);
    MSISValues[i] <- MSIS(M4Subset[[i]]$xx, testModel$upper, testModel$lower, M4Subset[[i]]$x);
}

# Profit!
mean(MSISValues)
