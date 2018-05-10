M4Full <- vector(mode="list",length(M4))
for(i in 1:length(M4Full)){
    M4Full[[i]]$x <- ts(c(M4[[i]]$x,M4[[i]]$xx),frequency=frequency(M4[[i]]$x));
    M4Full[[i]]$h <- M4[[i]]$h;
    M4Full[[i]]$st <- M4[[i]]$st;
    M4Full[[i]]$sn <- M4[[i]]$sn;
    M4Full[[i]]$n <- M4[[i]]$n;
    M4Full[[i]]$period <- M4[[i]]$period;
    M4Full[[i]]$type <- M4[[i]]$type;
    M4Full[[i]]$description <- M4[[i]]$description;
}

M4Full <- structure(M4Full,class="Mcomp")

save(M4Full,file="data/M4Full.Rdata")
