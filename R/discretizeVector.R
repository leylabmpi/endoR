discretizeVector <- function(l_var, return_all = FALSE){

    # sanity check
    if (length(l_var$thr) == 0) return(l_var) 
    
    # set category names
    numSplit <- length(l_var$thr)
    if (numSplit == 1){
        vNames <- c('Low', 'High')
    } else if (numSplit == 2){
        vNames <- c('Low', 'Medium', 'High')
    } else if (numSplit == 3){
        vNames <- c('Very_Low', 'Low', 'High' ,'Very_High')
    } else if (numSplit == 4){
        vNames <- c('Very_Low', 'Low', 'Medium', 'High' ,'Very_High')
    } else {vNames <- as.character(1:(numSplit+1))}
    
    
    newV <- rep(vNames[numSplit+1], length(l_var$var_v))
    
    for(j in seq(numSplit, 1, by = -1)){
        newV[ which(l_var$var_v <= l_var$thr[j]) ] <- vNames[j]
    }

   if (return_all == TRUE){
        l_var$med <- lapply(unique(newV), function(x, l_var){
            i <- which(newV == x)
            return(median(l_var$var_v[i]))
            }, l_var = l_var)
        names(l_var$med) <- unique(newV)
        l_var$var_d <- newV
        return(l_var)
    } else {
        return(newV)
    }
    
}