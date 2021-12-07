#' Extract a decision ensemble from a tree-based model, simplify it and creates an interaction network from it.
#'
#' Wrapper function to extract rules from a tree based model. 
#' It automatically transforms multiclass predictive variables into dummy variables. 
#' Optionally discretizes numeric variables (see discretizeDecisions). Measures decisions and optionally prunes them. Finally, generates a network. 
#' Can be run in parallel.
#'
#' @param model model to extract rules from.
#' @param model_type character string: 'RF', 'random forest', 'rf', 'xgboost', 'XGBOOST', 'xgb', 'XGB', 'ranger', 'Ranger', 'gbm' or 'GBM'.
#' @param data data with the same columns than data used to fit the model.
#' @param target response variable.
#' @param dummy_var if multiclass variables were transformed into dummy variables before fitting the model, one can pass their names in a vector here to avoid multiple levels to be used in a same rule (recommended).
#' @param classPos the positive class predicted by decisions
#' @param ntree number of trees to use from the model (default = all)
#' @param maxdepth maximal node depth to use for extracting rules (by default, full branches are used).
#' @param ... arguments to be passed to pruneDecisions, discretizeDecisions, filterDecisions.
#' @param exec if decisions have already been extracted, datatable with a 'condition' column.
#' @param light if FALSE, returns all intermediary decision ensembles; default = TRUE
#' @param in_parallel if TRUE, the function is run in parallel
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use, default is detectCores() - 1
#' @param cluster the cluster to use to run the function in parallel
#' @return A list with the final decision ensemble, if numeric variables were discretized in decisions, the discretized data, edges and nodes to make a network (plotNetwork and plotFeatures).
#' @import data.table
#' @export
model2DE <- function(
      model, model_type, data, target
      , ntree = 'all', maxdepth = Inf
      , classPos = NULL

      , dummy_var = NULL

      #discretization parameters
      , discretize = FALSE, K = 2, mode = 'data'

      #pruning parameters
      , prune = TRUE, maxDecay = 0.05, typeDecay = 2

      #aggregation parameters
      , aggregate_taxa = FALSE, taxa = NULL, type = 'both'

      #filter parameters
      , filter = TRUE, min_imp = 0.7, ntest = 100

      #parameters when in resampling
      , exec = NULL

      #parallelization
      , in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL

      #memory parameter
      , light = FALSE){


  
  # define classPos if it has not been passed
  if (is.character(target) && is.null(classPos) == TRUE) {
    classPos <- names(which.max(table(target)))
    cat('Positive class:', classPos, '\n')
  }

  # Start cluster for parallelization
  if (in_parallel == TRUE){
    if (is.null(cluster) == TRUE){
          message('Initiate parallelisation ... ')
          cluster <- makeCluster(n_cores)
          clusterEvalQ(cluster, library(endoR))
          clusterEvalQ(cluster, library(stringr))
          clusterEvalQ(cluster, library(dplyr))
          clusterEvalQ(cluster, library(inTrees))
          on.exit(stopCluster(cluster))
    }
  }

  # change class of data, otherwise bug with extractRules
  if ('data.table' %in% class(data)){data <- as.data.frame(data)}

  # settings
  dig_ori <- getOption('digits')
  options(digits = 15)
  on.exit(options(digits = dig_ori), add = TRUE)
  on.exit(return(res), add = TRUE)


  # Results list
  res <- list()
  

  ##### MODEL TO RULES #####
  if (is.null(exec) == TRUE){
    if (model_type %in% c('RF', 'random forest', 'rf')){
      treeList <- RF2List(rf = model)
    } else if (model_type %in% c('xgboost', 'XGBOOST', 'xgb', 'XGB')){
      treeList <- XGB2List(xgb = model, X = as.matrix(data))
    } else if (model_type %in% c('ranger', 'Ranger')){
      treeList <- Ranger2List_endoR(rf_ranger = model)
    } else if (model_type %in% c('gbm', 'GBM')){
      treeList <- GBM2List(gbm1 = model, X = as.matrix(data))
    } else {
      stop("model_type must be in:\n'RF', 'random forest', 'rf'\n'ranger', 'Ranger'\n'gbm', 'GBM'\n'xgboost', 'XGBOOST', 'xgb', 'XGB'\n")
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
                  , K = K, classPos=classPos, mode = mode
                  , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)

      res$data <- exec$data_ctg
      data <- exec$data_ctg
      exec <- exec$rules_ctg
    } else {
      # put into dummy
      exec <- decisions2FullDummy(rules = exec, data = data, in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
      data <- exec$data_dummy
      exec <- exec$rules
    }

    # re-order conditions, because I am not sure where sub-rules don't get properlly sorted... 
    exec <- exec[,condition:=sapply(condition, function(x){paste(sort(unlist(strsplit(x,split= " & "))),collapse = ' & ')})]
    exec <- unique(as.data.table(exec)[, n:= as.numeric(n)][, n := sum(n), by =  condition])

    if (light == FALSE){ 
      res$exec <- copy(exec) 
      res$data <- data
    }

  }


  ##### Calculate metrics #####
  rules <- getDecisionsMetrics(exec, data = data, target = target, classPos = classPos
                                , importances = FALSE
                                , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
  rules <- exec[rules, on = 'condition']
  if (light == FALSE){ res$rules_ori <- copy(rules) } 
  rm(exec)


  ##### Prune #####
  if (prune == TRUE){
    rules <- pruneDecisions(rules = rules,data = data, target = target, classPos = classPos
                        , maxDecay = maxDecay, typeDecay = typeDecay
                        , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
    if (light == FALSE){ res$rules_pruned <- copy(rules) }
  }



  ##### GET THE IMPORTANCES #####
  rules <- decisionImportance(rules = rules, data = data, target = target, classPos = classPos
                   , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster )
  if (light == FALSE){ 
    res$rules_imp <- copy(rules) 
  } else {res$n_decisions <- nrow(rules)} 



  ##### FILTER RULES #####
  if (filter == TRUE){
    rules <- filterDecisionsImportances(rules = rules, min_imp =min_imp )
    if (light == FALSE){ res$rules_filtered <- copy(rules) } 
  }
  
  if (light == TRUE){ res$rules <- copy(rules) } 



  ##### GET THE NETWORK #####
  coocc <- getNetwork(rules = rules, data = data, target = target, classPos = classPos
                    , aggregate_taxa = aggregate_taxa, taxa = taxa, type = type
                    , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
  if (aggregate_taxa == TRUE){res$newFeatures <- coocc$newFeatures}
  res$nodes <- coocc$nodes
  res$edges <- coocc$edges
  res$nodes_agg <- coocc$nodes_agg
  res$edges_agg <- coocc$edges_agg
  
  return(res)

}
