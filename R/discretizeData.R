#' Names of categories can optionally be given in knames.
#'
#' @param data data to discretize.
#' @param K numeric, number of categories (default: K = 2).
#' @param features vector with variables names or column numbers to discretize. If NULL (default), then all numeric variables are discretized.
#' @param knames optional: character vector of the same length than K, containing the ordered names for categories.
#' @param return_split if TRUE, then the table with thresholds used to discretize data is also returned.
#' @return Data with discretized variables.
#' @export
discretizeData <- function(data, K = 2, features = NULL, knames = NULL, return_split = FALSE){

    # set category names
    if (is.null(knames) == TRUE){
        if (K == 2){
          knames <- c('min','Low', 'High')
        } else if (K == 3){
          knames <- c('min','Low', 'Medium', 'High')
        } else if (K == 4){
          knames <- c('min','veryLow', 'Low', 'High', 'veryHigh')
        } else if (K == 5){
          knames <- c('min','veryLow', 'Low','Medium', 'High', 'veryHigh')
        } else { knames <- c("min", paste("L",seq(1:K),sep="")) }
    } else if (length(knames) == K){
        knames <- c('min', knames)
    }

    # get column numbers - not the most efficient but later easier
    if (is.character(features) == TRUE) {
        colNb <- which(colnames(data) %in% features)
    } else if (is.numeric(features) == TRUE) {
        colNb <- features
    } else {
        colNb <- which(sapply(data, function(x){ (length(unique(x)) >2 & is.numeric(x)) }))
    }

    # discretize
    data_ctg <- as.data.table(data)
    tmp <- lapply(data_ctg[, (colNb), with=FALSE], discretizeVector,K=K,knames=knames,return_all=TRUE)
    names(tmp) <- as.character(colNb)
    for (j in colNb){
        set(data_ctg, i = NULL, j=j, tmp[[as.character(j)]]$newV)
        set(data_ctg, i =  which(data_ctg[,j, with = FALSE] == ""), j = j, value = NA)
    }
    data_ctg <- data_ctg[, c(colNb):= lapply(.SD, as.factor), .SDcols = colNb]


    # get the split table if asked
    if (return_split == TRUE){
        splitV <- list()
        for (i in 1:length(tmp)){
            splitV[[i]] <- tmp[[i]]$splitV
        }
        names(splitV)<- names(tmp)

        splitV_med <- lapply(names(splitV), FUN = getMedian, data = data, splitV = splitV)
        names(splitV_med)<- names(splitV)

        return(list('data_ctg' = data_ctg, 'splitV' = splitV, 'splitV_med' = splitV_med))
}



##############
getMedian <- function(var, data, splitV){
    dataVar <- data[[as.numeric(var)]]
    med <- list()
    for ( i in 2:length(splitV[[var]]) ){
        iX <- which(dataVar >= splitV[[var]][i-1] & dataVar <= splitV[[var]][i])
        med <- c(med, median(dataVar[iX], na.rm = TRUE))
    }
    names(med) <- names(splitV[[var]])[-1]
    return(med)
} 