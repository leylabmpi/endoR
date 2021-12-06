#' Discretize numerical variables in a dataset based on thresholds used in the model to create splits.
#'
#' This function discretizes all numerical variables into Kmax categories based on the splits used by the model for each variable. 
#'
#' @param data data to discretize.
#' @param Kmax numeric, maximal number of categories for each variable (default: Kmax = 2).
#' @param return_split if TRUE, then the table with thresholds used to discretize data is also returned.
#' @return Data with discretized variables.
#' @export
discretizeData_model <- function(data, conditions, Kmax = 2, return_split = FALSE){

    # get the thresholds
    l_var <- getThresholds(conditions = conditions, data = data, Kmax = Kmax)
    colNb <- names(l_var)

    # discretize the variable vectors
    l_var <- lapply(l_var, discretizeVector_model, return_all = TRUE)
    names(l_var) <- colNb
    colNb <- as.integer(colNb)

    data_ctg <- as.data.table(data)

    for (j in colNb) set(data_ctg, i = NULL, j=j, l_var[[as.character(j)]]$var_d)
    
    data_ctg <- data_ctg[, (colNb):= lapply(.SD, as.factor), .SDcols = colNb]


    # get the split table if asked
    if (return_split == TRUE){
        splitV <- lapply(l_var, function(x){x$thr})
        names(splitV)<- names(l_var)

        splitV_med <- lapply(l_var, function(x){x$med})
        names(splitV_med)<- names(splitV)

        return(list('data_ctg' = data_ctg, 'splitV' = splitV, 'splitV_med' = splitV_med))
    }


    return(data_ctg)
}


