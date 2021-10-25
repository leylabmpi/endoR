#' Extract decisions from a model and create bootstrap resamples.
#'
#' to run before bootstrapping on parallel with the clustermq package and model2DE_cluster function. Extracts decisions, optionally discretizes them. Creates data partitions for bootstrapping.
#'
#' @param model model to extract rules from.
#' @param model_type character string: 'RF', 'random forest', 'rf', 'xgboost', 'XGBOOST', 'xgb', 'XGB', 'ranger', 'Ranger', 'gbm' or 'GBM'.
#' @param data data with the same columns than data used to fit the model.
#' @param target response variable.
#' @param dummy_var if multiclass variables were transformed into dummy variables before fitting the model, one can pass their names in a vector here to avoid multiple levels to be used in a same rule (recommended).
#' @param classPos the positive class predicted by decisions
#' @param times number of bootstraps
#' @param p fraction of data to resample.
#' @param sample_weight numeric vector with the weights of samples for bootstrap resampling. For classification, if 2 values are given, the 1st one is assumed to be for the positive class (classpos argument).
#' @param ntree number of trees to use from the model (default = all)
#' @param maxdepth maximal node depth to use for extracting rules (by default, full branches are used).
#' @param discretize if TRUE, discretization is performed with the K and features_ctg parameters (discretizeDecisions, by default = FALSE).
#' @param in_parallel if TRUE, the function is run in parallel.
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use.
#' @param cluster the cluster to use to run the function in parallel.
#' @return A list with the row numbers of partitioned data, the rules originally extracted from the model and new data if discretization was performed.
#' @import data.table
#' @export
preCluster <- function(
    model, model_type, data, target
    , times = 10, p = .5, sample_weight = NULL, classPos = NULL
    , ntree = 'all', maxdepth = Inf, dummy_var = NULL
    , discretize = FALSE, K = 2, features_ctg = NULL
    , seed = 0
    , in_parallel = FALSE, n_cores = detectCores() - 1
){

  res <- list()

  #### Create the data partition
  if (length(sample_weight) == 2){
    sample_weight <- ifelse(target == classPos, sample_weight[1], sample_weight[2])
  } else if (is.null(sample_weight)){ 
    sample_weight <- rep(1, length(target)) 
  } 

  np <- floor(p*nrow(data))
  res$partitions <- list()
  for (i in 1:times){
    set.seed(i)
    res$partitions[[i]] <- sample( 1:length(target), size = np, replace = TRUE, prob = sample_weight )
  }
  


  # change class of data, otherwise bug with extractRules
  if ('data.table' %in% class(data)){data <- as.data.frame(data)}

  if (model_type %in% c('RF', 'random forest', 'rf')){
    treeList <- RF2List(model)
  } else if (model_type %in% c('xgboost', 'XGBOOST', 'xgb', 'XGB')){
    treeList <- XGB2List(model, as.matrix(data))
  } else if (model_type %in% c('ranger', 'Ranger')){
    treeList <- Ranger2List_endoR(rf_ranger = model)
  } else if (model_type %in% c('gbm', 'GBM')){
    treeList <- GBM2List(model, as.matrix(data))
  } else {
    stop("model_type must be in:\n'RF', 'random forest', 'rf'\n'ranger', 'Ranger'\n'gbm', 'GBM'\n'xgboost', 'XGBOOST', 'xgb', 'XGB'\n")
  }

  if (in_parallel == TRUE){
    cluster <- makeCluster(n_cores)
    clusterEvalQ(cluster, library(endoR))
    clusterEvalQ(cluster, library(stringr))
    clusterEvalQ(cluster, library(dplyr))
    clusterEvalQ(cluster, library(inTrees))
    on.exit(stopCluster(cluster))
  }

  if (ntree == 'all'){ntree <- treeList[[1]]}
  message('Extract rules...')
  exec <- extractDecisions(treeList, X=data, ntree = ntree, maxdepth = maxdepth, in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
  exec <- unique(as.data.table(exec)[,'n':=.N, by = 'condition'])

  if(!is.null(dummy_var)){
    exec <- changeDecisionsDummies(rules = exec, dummy_var = dummy_var, data = data, target =target, classPos = classPos
                               , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
  }

  if (discretize == TRUE){
    exec <- discretizeDecisions(rules = exec, data = data, target=target
                  , K = K, classPos=classPos
                  , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)

    res$data <- exec$data_ctg
    data <- exec$data_ctg
    res$exec <- exec$rules_ctg
  } else {
    # put into dummy
    exec <- decisions2FullDummy(rules = exec, data = data, in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
    res$data <- exec$data_ctg
    res$exec <- exec$rules
  }

  # re-order conditions, because I am not sure where sub-rules don't get properlly sorted... 
  res$exec <- res$exec[,condition:=sapply(condition, function(x){paste(sort(unlist(strsplit(x,split= " & "))),collapse = ' & ')})]
  res$exec <- unique(as.data.table(res$exec)[, n:= as.numeric(n)][, n := sum(n), by =  condition])

  return(res)

}
