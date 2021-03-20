#' Prune rules of a decision ensemble
#'
#' This function removes from rules, variables that do not increase the rule error more than a certain threshold. See the pruneRules function from the inTrees package (Deng, 2019) for more details.
#'
#' @param rules a data frame with a column "condition".
#' @param data data to use for calculating the metrics.
#' @param target response variable.
#' @param maxDecay threshold for the increase in error; if maxDecay = -Inf, no pruning is done; if maxDecay = 0, only variables increasing the error are pruned from decisions.
#' @param typeDecay if typeDecay = 1, the absolute increase in error is computed, and the relative one is computed if typeDecay = 2 (default).
#' @param classPos for classification tasks, the positive class predicted.
#' @param in_parallel if TRUE, the function is run in parallel.
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use, default is detectCores() - 1.
#' @param cluster the cluster to use to run the function in parallel.
#' @return Decision ensemble with pruned conditions.
#' @export
pruneDecisions <- function(rules, data, target, classPos = NULL, maxDecay = 0, typeDecay = 1
                          , in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL
                          ){

  # get the classPos
  if (is.numeric(target) == FALSE){
    target <- ifelse(target == classPos, 1, 0)
    type <- 'classification'
  } else {type <- 'reg'}

  tmp <- split(as.matrix(rules), seq(nrow(rules)))
  tmp <- lapply(tmp, FUN = function(x, newN){names(x) <- newN; return(x)}
                , newN = colnames(rules))
  

  #### Run the pruning
  if (in_parallel == FALSE){
    newRuleMetric <- lapply(tmp, FUN = pruneSingleRule_endoR
                             , data=data,target=target, type = type
                             , maxDecay = maxDecay, typeDecay = typeDecay)

  } else {
    if (is.null(cluster) == TRUE){
      message('Initiate parallelisation ... ')
      cluster <- makeCluster(n_cores)
      clusterEvalQ(cluster, library(babR))
      clusterEvalQ(cluster, library(stringr))
      on.exit(stopCluster(cluster))
    }

    message('Pruning ... ')
    newRuleMetric <- parLapply(cl = cluster, tmp, fun = pruneSingleRule_endoR
                              , data=data,target=target, type = type
                              , maxDecay = maxDecay, typeDecay = typeDecay)
  }

  # Count and remove duplicates
  newRuleMetric <- do.call(what = rbind, newRuleMetric)
  newRuleMetric <- as.data.table(newRuleMetric)[
           , c('len','support','err','pred'):=lapply(.SD, as.numeric)
           , .SDcols = c('len','support','err','pred')]
  
  # re-order conditions, before just to be sure! 
  newRuleMetric <- newRuleMetric[,condition:=sapply(condition, function(x){paste(sort(unlist(strsplit(x,split= " & "))),collapse = ' & ')})]
  
  if ('n' %in% colnames(newRuleMetric)){
    newRuleMetric <- unique(as.data.table(newRuleMetric)[, n:= as.numeric(n)][, n := sum(n), by =  condition])
  } else {
    newRuleMetric <- unique(as.data.table(newRuleMetric)[, n:= .N, by =  condition])
  }

  return(newRuleMetric)

}


##########################################################

#' @export
pruneSingleRule_endoR <-
function(rule, data, target, type='reg', maxDecay = 0, typeDecay = 1){
  setDTthreads(threads = 1)

  errOrig <- as.numeric(rule["err"])
  ruleV <- unlist(strsplit(rule["condition"],split= " & "))
  if(length(ruleV)==1) return(rule)
  
  toTestRules <- c()
  lenRules <- c()
  for (i in 1:(length(ruleV) - 1)){
    tmp <- combn(x = ruleV, m = i)
    tmp <- asplit(tmp, MARGIN = 2)
    tmp <- unlist(lapply(tmp, FUN = function(x){paste(sort(x),collapse = ' & ')})) #order here
    toTestRules <- c(toTestRules, tmp)
    lenRules <- c(lenRules, rep(i, length(tmp)))
  }
  toTestRules <- data.table(len = lenRules, 'condition' = toTestRules)
  
  li <- 1
  while (li < length(ruleV)){
    metricsTmp <- apply(subset(toTestRules, len == li, select = condition), MARGIN = 1
                         , measureSingleDecision, data = data, target = target, type = type)
    metricsTmp <- t(metricsTmp)
    metricsTmp <- as.data.table(metricsTmp[metricsTmp[,'len'] != '-1', ])[
      , c('len','err'):=lapply(.SD, as.numeric), .SDcols = c('len','err')]
    
    if (typeDecay == 1) {
      metricsTmp <- metricsTmp[, 'decay':=err - errOrig]
    } else {
      metricsTmp <- metricsTmp[, 'decay':=(err - errOrig)/max(errOrig, 1e-6)]
    }
    metricsTmp <- subset(metricsTmp, decay <= maxDecay)
    if (nrow(metricsTmp) >0){
      setorder(metricsTmp, len, err)
      tmp <- unlist(metricsTmp[,decay:=NULL][1,])
      rule[names(tmp)] <- tmp
      li <- Inf
    } else {li <- li+1}
  }


  return(rule)
}