#' Discretize numerical variables in decision ensemble
#'
#' This function replaces in a decision ensemble the boundaries of numerical features by their corresponding levels when the variable is discretized.
#' If discretized data are not passed, data are first discretized into Kmax categories based on their quantiles (see discretizeData). 
#' The error, prediction, importance and multiplicity of decisions are updated after discretization.
#'
#' @param rules a data frame with a column "condition".
#' @param data data to discretize.
#' @param target response variable. 
#' @param classPos for classification, the positive class.
#' @param mode whether to discretize variables based on the data distribution (default, mode = 'data') or on the data splits in the model (mode = 'model').
#' @param Kmax numeric, maximal number of categories to create from numeric variables (default: Kmax = 2).
#' @param splitV instead of running internally discretizeData, one can provide a list with, for each variable to discretize in rules, the thresholds delimiting each new category.
#' @param data_ctg discretized data, if splitV is passed. Necessary to re-compute the metrics (if column 'err' in rules).
#' @param return_data if TRUE, discretized data are also returned.
#' @param in_parallel if TRUE, the function is run in parallel.
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use, default is detectCores() - 1.
#' @param cluster the cluster to use to run the function in parallel.
#'
#'  @return Decision ensemble with discretized variables in the condition. Decisions with the same condition are aggregated: their importances are summed, and all other metrics are averaged.
#'
#'  @export
discretizeDecisions <- function(rules, data = NULL, target, mode = 'data'
                            , Kmax = 2, splitV = NULL
                            , classPos = NULL 
                            , in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL){

    ### discretize data if needed
    if (is.null(splitV) == TRUE){
        message('Discretise data')
        if (mode == 'model'){
            data <- discretizeData_model(data = data, conditions = rules$condition, Kmax = K, return_split = TRUE)
        } else {
            data <- discretizeData(data = data, conditions = rules$condition, K = K, return_split = TRUE)
        }
        splitV <- data$splitV_med
        data <- data$data_ctg
    } 
    
    
    message('Discretise rules')
    ### discretize rules
    if (in_parallel == FALSE){
        newRuleMetric <- t(apply(rules, MARGIN = 1, FUN = discretizeSingleRule, splitV = splitV))
    } else {
        if (is.null(cluster) == TRUE){
          message('Initiate parallelisation ... ')
          cluster <- makeCluster(n_cores)
          on.exit(stopCluster(cluster))
          clusterEvalQ(cluster, library(endoR))
          clusterEvalQ(cluster, library(dplyr))
          clusterEvalQ(cluster, library(stringr))
        }
        newRuleMetric <- parApply(cl = cluster, rules, MARGIN = 1, FUN = discretizeSingleRule, splitV = splitV)
        newRuleMetric <- t(newRuleMetric)

    }

    newRuleMetric <- newRuleMetric[complete.cases(newRuleMetric),]
    if ('character' %in% class(newRuleMetric)){
        newRuleMetric <- t(newRuleMetric)
    } else if(nrow(newRuleMetric) == 0){ 
        return(list('rules_ctg' = newRuleMetric, 'data_ctg' = data)) 
    } 

    # Count and remove duplicates
    if ('err' %in% colnames(newRuleMetric)){
        newRuleMetric <- as.data.table(newRuleMetric)[
           , c('len','support','err','pred'):=lapply(.SD, as.numeric)
           , .SDcols = c('len','support','err','pred')]
        if ('n' %in% colnames(newRuleMetric)){
            newRuleMetric <- unique(as.data.table(newRuleMetric)[, n:= as.numeric(n)][, n := sum(n), by =  condition][
                                to_update == 'yes', c('len', 'support', 'err', 'pred'):= NA])
        } else {
            newRuleMetric <- unique(as.data.table(newRuleMetric)[, n:= .N, by =  condition][
                                to_update == 'yes', c('len', 'support', 'err', 'pred'):= NA])
        }
        setnames(newRuleMetric, old = 'to_update', new = 'to_update_ctg')
    } else {
        newRuleMetric <- as.data.table(newRuleMetric)
        if ('n' %in% colnames(newRuleMetric)){
            newRuleMetric <- unique(as.data.table(newRuleMetric)[, n:= as.numeric(n)][, n := sum(n), by =  condition][, to_update:= NULL])
        } else {
            newRuleMetric <- unique(as.data.table(newRuleMetric)[, n:= .N, by =  condition][, to_update:= NULL])
        }
    }
    
    # Transform into dummy
    newRuleMetric <- decisions2FullDummy(rules = newRuleMetric, data = data, in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
    data <- newRuleMetric$data_dummy
    newRuleMetric <- as.data.table(newRuleMetric$rules)
    

    # Get the new metrics
    if (nrow(newRuleMetric[to_update == 'yes',]) > 0 & 'err' %in% colnames(newRuleMetric)){
        
        if ('imp' %in% colnames(newRuleMetric)){
            importances <- TRUE
            colN <- c('len','err','support','pred', 'imp', 'to_update', 'to_update_ctg')
        } else { 
            importances <- FALSE 
            colN <- c('len','err','support','pred' ,'to_update', 'to_update_ctg')
        }
        
        no_up <- unique(subset(newRuleMetric, to_update == 'no' & to_update_ctg == 'no'
                        , select = -c(to_update, to_update_ctg)))
        tmp1 <- unique(subset(newRuleMetric, (to_update == 'yes' | to_update_ctg == 'yes') 
                              & !(condition %in% no_up$condition))[, (colN):=NULL]
                         )

        tmp2 <-  getDecisionsMetrics(tmp1
                              , data = data, target = target, classPos = classPos
                              , importances = importances
                              , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
        tmp2 <- tmp1[tmp2, on = 'condition']
        
        if (nrow(no_up) > 0 ){
            newRuleMetric <- rbind(no_up, tmp2)
        } else {
            newRuleMetric <- tmp2
        }

        newRuleMetric <- newRuleMetric[, c('len','support','err','pred', 'n'):=lapply(.SD, as.numeric)
                                       , .SDcols = c('len','support','err','pred', 'n')]
        newRuleMetric <- unique(newRuleMetric[, n := sum(n), by =  condition])
    
    } else { newRuleMetric <- unique(newRuleMetric[,to_update:=NULL]) }

    
    return(list('rules_ctg' = newRuleMetric, 'data_ctg' = data ))
    
}



#####################################################
discretizeSingleRule <- function(rule, splitV){
    
    # get the variables involved in the rule
    v <- unlist(str_extract_all(rule['condition'], pattern = '(?<=\\[,)[:digit:]+(?=])')) 
    
    if (length(v[v%in%names(splitV)]) > 0){
        to_update <- 'yes'
        # make sub-conditions
        oriRule <- unlist(strsplit(rule['condition'],split= " & "))
        tmp <- unlist(str_extract(oriRule, pattern = '(?<=\\[,)[:digit:]+(?=])'))
        names(oriRule) <- tmp

        # get new sub-condition for discretized v
        newRule <- oriRule[which(names(oriRule)%in%names(splitV))]
        newRule <- sapply(unique(names(newRule)), getNewRule, rule = newRule, splitV = splitV)
         
        # merge all 
        newRule <- unlist(c(newRule, oriRule[which(!(names(oriRule)%in%names(splitV)) )]))
        if (length(newRule) < length(oriRule)){
            rule['condition'] <- NA
        } else {
            #newRule <- newRule[order(names(newRule))]
            newRule <- paste(sort(newRule), collapse = ' & ')
            rule['condition'] <- newRule
        }
    } else { to_update <- 'no'}

    return(c(rule, 'to_update' = to_update))
}


###################################################
#' @export
getNewRule <-  function(v, rule, splitV){
    vIx <- which(names(rule) == v)
    
    thr <- as.numeric(str_extract(rule[vIx], pattern = "(?<=\\](>|(<=))).*$"))
    sign <- str_extract(rule[vIx], pattern = ">|(<=)")
    ctg <- c()
    for (i in 1:length(thr)){
        if (sign[i] == '<='){
            ctg <- c(ctg, names(splitV[[v]])[which(splitV[[v]] <= thr[i])])
        } else {
            ctg <- c(ctg, names(splitV[[v]])[which(splitV[[v]] >= thr[i])])
        }
    }

    if (length(thr) >1){ctg <- ctg[duplicated(ctg)]}
    if (length(ctg) == 0){return(NULL)}

    ctg <- paste("'", ctg,"'", sep = "")
    deb <- unique(str_extract(rule[vIx], pattern = ".*(?=>|(<=))"))
    newX <- paste0(deb, ' %in% c(', paste(ctg, collapse = ','), ')')
    return(newX)
}

