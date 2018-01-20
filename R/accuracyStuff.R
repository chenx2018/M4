errorMeasurer <- function(holdout, forecast, actuals){
    # holdout - the actual data from the holdout 
    # forecast - point forecasts
    # actuals - the actual data from the in-sample
    SMAPE <- 2*mean(abs(holdout-forecast) / (abs(holdout)+abs(forecast)));
    MASE <- mean(abs(holdout-forecast)) / mean(abs(diff(actuals,lag=frequency(actuals))));
    errors <- c(SMAPE,MASE);
    names(errors) <- c("SMAPE","MASE");
    return(errors);
}

OWA <- function(errorsFromModel, errorsFromNaive2){
    # errorsFromModel - matrix with SMAPE and MASE from your model applied to some subset
    # errorsFromNaive2 - matrix with SMAPE and MASE from Naive2 applied to the same subset
    errorsFromModel <- apply(errorsFromModel,2,mean);
    errorsFromNaive2 <- apply(errorsFromNaive2,2,mean);
    
    errors <- errorsFromModel / errorsFromNaive2;
    return(mean(errors));
}

MSIS <- function(holdout, upper, lower, actuals){
    # holdout - the actual data from the holdout 
    # upper - upper bound
    # lower - lower bound
    # actuals - the actual data from the in-sample
    scale <- 2/0.05;
    MSIS <- ((mean(upper - lower) +
                  scale*(mean((lower - holdout)*(holdout < lower)) + 
                             mean((holdout - upper)*(holdout > upper)))) / 
                 mean(abs(diff(actuals,lag=frequency(actuals)))));

    return(MSIS);
}
