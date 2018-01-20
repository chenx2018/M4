nOverall <- rep(NA,length(M4))
for(i in 1:length(M4)){
    nOverall[i] <- M4[[i]]$n
}
hist(nOverall)
quantile(nOverall,c(0:10)/10)
