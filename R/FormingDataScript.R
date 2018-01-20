library(readr)

M4 <- list();
m <- 1;
dataTypes <- c("DEMOGRAPHIC","FINANCE","INDUSTRIAL","MACRO","MICRO","OTHER");

# Yearly data
Yearly_train <- read_csv("Data/Yearly-train.csv");
typesQuantity <- c(1088,6519,3716,3903,6538,1236);
dataFreq <- 1;
h <- 6;
for(i in 1:nrow(Yearly_train)){
    M4[[paste0("Y",sprintf("%05d",i))]] <- list();
    x <- as.numeric(Yearly_train[i,]);
    x <- ts(x[!is.na(x)],frequency=dataFreq);
    n <- length(x);
    M4[[m]]$x <- ts(x[1:(n-h)],frequency=dataFreq);
    M4[[m]]$xx <- ts(x[(n-h+1):n],start=time(x)[n-h]+deltat(x),frequency=dataFreq);
    M4[[m]]$h <- h;
    M4[[m]]$st <- paste0("Y",sprintf("%05d",i));
    M4[[m]]$n <- n;
    M4[[m]]$sn <- paste0("N",sprintf("%06d",m));
    M4[[m]]$period <- "YEARLY";
    M4[[m]]$type <- dataTypes[which((i-1) < cumsum(typesQuantity))[1]]
    M4[[m]]$description <- NA;
    M4[[m]] <- structure(M4[[m]],class="Mdata")
    
    m <- m + 1;
}
rm(Yearly_train)

# Quarterly data
Quarterly_train <- read_csv("Data/Quarterly-train.csv")
typesQuantity <- c(1858,5305,4637,5315,6020,865);
dataFreq <- 4;
h <- 8;
for(i in 1:nrow(Quarterly_train)){
    M4[[paste0("Q",sprintf("%05d",i))]] <- list();
    x <- as.numeric(Quarterly_train[i,]);
    x <- ts(x[!is.na(x)],frequency=dataFreq);
    n <- length(x);
    M4[[m]]$x <- ts(x[1:(n-h)],frequency=dataFreq);
    M4[[m]]$xx <- ts(x[(n-h+1):n],start=time(x)[n-h]+deltat(x),frequency=dataFreq);
    M4[[m]]$h <- h;
    M4[[m]]$st <- paste0("Q",sprintf("%05d",i));
    M4[[m]]$n <- n;
    M4[[m]]$sn <- paste0("N",sprintf("%06d",m));
    M4[[m]]$period <- "QUARTERLY";
    M4[[m]]$type <- dataTypes[which((i-1) < cumsum(typesQuantity))[1]]
    M4[[m]]$description <- NA;
    M4[[m]] <- structure(M4[[m]],class="Mdata")
    
    m <- m + 1;
}
rm(Quarterly_train)

# Monthly data
Monthly_train <- read_csv("Data/Monthly-train.csv")
typesQuantity <- c(5728,10987,10017,10016,10975,277);
dataFreq <- 12;
h <- 18;
for(i in 1:nrow(Monthly_train)){
    M4[[paste0("M",sprintf("%05d",i))]] <- list();
    x <- as.numeric(Monthly_train[i,]);
    x <- ts(x[!is.na(x)],frequency=dataFreq);
    n <- length(x);
    M4[[m]]$x <- ts(x[1:(n-h)],frequency=dataFreq);
    M4[[m]]$xx <- ts(x[(n-h+1):n],start=time(x)[n-h]+deltat(x),frequency=dataFreq);
    M4[[m]]$h <- h;
    M4[[m]]$st <- paste0("M",sprintf("%05d",i));
    M4[[m]]$n <- n;
    M4[[m]]$sn <- paste0("N",sprintf("%06d",m));
    M4[[m]]$period <- "MONTHLY";
    M4[[m]]$type <- dataTypes[which((i-1) < cumsum(typesQuantity))[1]]
    M4[[m]]$description <- NA;
    M4[[m]] <- structure(M4[[m]],class="Mdata")
    
    m <- m + 1;
}
rm(Monthly_train)

# Weekly data
Weekly_train <- read_csv("Data/Weekly-train.csv")
typesQuantity <- c(24,164,6,41,112,12);
dataFreq <- 52;
h <- 13;
for(i in 1:nrow(Weekly_train)){
    M4[[paste0("W",sprintf("%05d",i))]] <- list();
    x <- as.numeric(Weekly_train[i,]);
    x <- ts(x[!is.na(x)],frequency=dataFreq);
    n <- length(x);
    M4[[m]]$x <- ts(x[1:(n-h)],frequency=dataFreq);
    M4[[m]]$xx <- ts(x[(n-h+1):n],start=time(x)[n-h]+deltat(x),frequency=dataFreq);
    M4[[m]]$h <- h;
    M4[[m]]$st <- paste0("W",sprintf("%05d",i));
    M4[[m]]$n <- n;
    M4[[m]]$sn <- paste0("N",sprintf("%06d",m));
    M4[[m]]$period <- "WEEKLY";
    M4[[m]]$type <- dataTypes[which((i-1) < cumsum(typesQuantity))[1]]
    M4[[m]]$description <- NA;
    M4[[m]] <- structure(M4[[m]],class="Mdata")
    
    m <- m + 1;
}
rm(Weekly_train)

# Daily data
Daily_train <- read_csv("Data/Daily-train.csv")
typesQuantity <- c(10,1559,422,127,1476,633);
dataFreq <- 365;
h <- 14;
for(i in 1:nrow(Daily_train)){
    M4[[paste0("D",sprintf("%05d",i))]] <- list();
    x <- as.numeric(Daily_train[i,]);
    x <- ts(x[!is.na(x)],frequency=dataFreq);
    n <- length(x);
    M4[[m]]$x <- ts(x[1:(n-h)],frequency=dataFreq);
    M4[[m]]$xx <- ts(x[(n-h+1):n],start=time(x)[n-h]+deltat(x),frequency=dataFreq);
    M4[[m]]$h <- h;
    M4[[m]]$st <- paste0("D",sprintf("%05d",i));
    M4[[m]]$n <- n;
    M4[[m]]$sn <- paste0("N",sprintf("%06d",m));
    M4[[m]]$period <- "DAILY";
    M4[[m]]$type <- dataTypes[which((i-1) < cumsum(typesQuantity))[1]]
    M4[[m]]$description <- NA;
    M4[[m]] <- structure(M4[[m]],class="Mdata")
    
    m <- m + 1;
}
rm(Daily_train)

# Hourly data
Hourly_train <- read_csv("Data/Hourly-train.csv")
typesQuantity <- c(0,0,0,0,0,414);
dataFreq <- 24;
h <- 48;
for(i in 1:nrow(Hourly_train)){
    M4[[paste0("H",sprintf("%05d",i))]] <- list();
    x <- as.numeric(Hourly_train[i,]);
    x <- ts(x[!is.na(x)],frequency=dataFreq);
    n <- length(x);
    M4[[m]]$x <- ts(x[1:(n-h)],frequency=dataFreq);
    M4[[m]]$xx <- ts(x[(n-h+1):n],start=time(x)[n-h]+deltat(x),frequency=dataFreq);
    M4[[m]]$h <- h;
    M4[[m]]$st <- paste0("H",sprintf("%05d",i));
    M4[[m]]$n <- n;
    M4[[m]]$sn <- paste0("N",sprintf("%06d",m));
    M4[[m]]$period <- "HOURLY";
    M4[[m]]$type <- dataTypes[which((i-1) < cumsum(typesQuantity))[1]]
    M4[[m]]$description <- NA;
    M4[[m]] <- structure(M4[[m]],class="Mdata")
    
    m <- m + 1;
}
rm(Hourly_train)

M4 <- structure(M4,class="Mcomp")

save(M4,file="M4.Rdata")
