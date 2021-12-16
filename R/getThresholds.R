#' Get thresholds to discretize variables according to their split in tree ensemble models
#' @export

getThresholds <- function(conditions, data, Kmax = 2){
	### a little slow, may be improved 

	# get all individual sub conditions per variable
	var_cond <- unlist( lapply(conditions, function(x){unlist(strsplit(x, split = ' & '))}) )
	# make it to a data.frame with variable and thresholds used
	var_cond <- data.frame(var = as.numeric(str_extract(var_cond, pattern = '[:digit:]*(?=\\])'))
                 , thr = as.numeric(str_extract(var_cond, pattern = '-?[:digit:]*\\.?[:digit:]*$'))
                          )

	# and now transform to a list 
	var_split <- lapply(sort(unique(var_cond$var))
	                    , function(x, var_cond, data){
	                    	thr <- unlist(subset(var_cond, var == x, select = thr))
	                    	var_v <- data[[x]]
	                    	thr[thr < min(var_v)] <- min(var_v)
	                    	thr[thr > max(var_v)] <- max(var_v)
	                    	return(thr)
	                    },var_cond = var_cond, data = data)
	names(var_split) <- as.character(sort(unique(var_cond$var)))

	# remove non-numeric variables
	non_num <- sapply(data, function(x){length(unique(x))>2})
	var_split <- var_split[non_num]

	# get thresholds for discretization
	if (Kmax == 2){
		new_thr <- lapply(var_split, getMode)
	} else {
		new_thr <- lapply(var_split, getModes_all)
		new_thr <- lapply(new_thr, function(x, Kmax){x[1:min(Kmax-1, length(x))]}, Kmax = Kmax)
	}
	
	# remove thresholds out of range
	colNb <- as.integer(names(new_thr))
	new_thr <- lapply(colNb, function(x, data, thr){list('var_v' = data[[x]]
													  , 'thr' = thr[[as.character(x)]]
                                                )}, data = data, thr = new_thr)

    new_thr <- lapply(new_thr, function(x){ 
    	x$thr <- x$thr[ x$thr >= min(x$var_v) & x$thr <= max(x$var_v)] 
    	if (length( x$thr ) == 0) x$thr <- min(x$var_v)
    	return(x)
    	})
    names(new_thr) <- as.character(colNb)

	return(new_thr)
}


##########

getMode <- function(x){
	if (length(x) == 1){
		return(x)
	} else{
		tmp <- density(x)
		return(tmp$x[which.max(tmp$y)])
	}
}


getModes_all <- function(var){
    # copied from the pastecs R-package: 
    # https://github.com/phgrosjean/pastecs/blob/master/R/turnpoints.R
    # (I just removed the unecessary bits..)
    
    if (length(x) == 1){
		return(x)
	}
	
    x <- as.vector(density(var)$y)
    n <- length(x)
    diffs <- c(x[1] - 1, x[1:(n - 1)]) != x
    
    uniques <- x[diffs]
    
    n2 <- length(uniques)
    poss <- (1:n)[diffs]
    exaequos <- c(poss[2:n2], n + 1) - poss - 1
    
    m <- n2 - 2
    ex <- matrix(uniques[1:m + rep(3:1, rep(m, 3)) - 1], m)
    peaks <- c(FALSE, apply(ex, 1, max, na.rm = TRUE) == ex[, 2], FALSE)
    tppos <- (poss + exaequos)[peaks]
    
    # Now, order the peaks and return the x values 
    y_peaks <- x[tppos]
	x_peaks <- density(var)$x[tppos]

    peaks_order <- order(y_peaks, decreasing = TRUE)
    #y_peaks <- y_peaks[peaks_order]
    #x_peaks <- x_peaks[peaks_order]
    return(x_peaks[peaks_order])
}