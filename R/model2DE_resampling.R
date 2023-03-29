#' Run model2DE on several bootstrap resamples.
#'
#' Wrapper around the model2DE function to run it on several bootstrap resamples.
#'
#' @param model model to extract rules from.
#' @param model_type character string: 'RF', 'random forest', 'rf', 'xgboost', 'XGBOOST', 'xgb', 'XGB', 'ranger', 'Ranger', 'gbm' or 'GBM'.
#' @param data data with the same columns than data used to fit the model.
#' @param target response variable.
#' @param classPos the positive class predicted by decisions
#' @param times number of bootstraps
#' @param p fraction of data to resample.
#' @param sample_weight numeric vector with the weights of samples for bootstrap resampling. For classification, if 2 values are given, the 1st one is assumed to be for the positive class (classpos argument).
#' @param ntree number of trees to use from the model (default = all)
#' @param maxdepth maximal node depth to use for extracting rules (by default, full branches are used).
#' @param dummy_var if multiclass variables were transformed into dummy variables before fitting the model, one can pass their names in a vector here to avoid multiple levels to be used in a same rule (recommended).
#' @param prune should unimportant features be removed from decisions (= pruning)? Features are removed by calculating the difference in prediction error of the decision with versus without the feature. If the difference is small (< maxDecay), then the feature is removed. The difference can be absolute (typeDecay = 1) or relative (typeDecay = 2, default). See pruneDecisions() for details.
#' @param maxDecay when pruning, threshold for the increase in error; if maxDecay = -Inf, no pruning is done; if maxDecay = 0, only variables increasing the error are pruned from decisions.
#' @param typeDecay if typeDecay = 1, the absolute increase in error is computed, and the relative one is computed if typeDecay = 2 (default).
#' @param discretize should numeric variables be transformed to categorical variables? If TRUE, K categories are created for each variable based on their distribution in data (mode = 'data') or based on the thresholds used in the decision ensemble (mode = 'model')
#' @param K numeric, number of categories to create from numeric variables (default: K = 2).
#' @param mode whether to discretize variables based on the data distribution (default, mode = 'data') or on the data splits in the model (mode = 'model').
#' @param filter should decisions with low importance be removed from the decision ensemble? If TRUE, then decisions are filtered in a heuristic manner according to their importance and multiplicity (see filterDecisionsImportances() ).
#' @param min_imp minimal relative importance of the decisions that must be kept, the threshold to remove decisions is thus going to take lower values than max(imp)*min_imp.
#' @param seed which seed to use to make the random bootstraps - it is fixed for reproducibility
#' @param in_parallel if TRUE, the function is run in parallel
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use, default is detectCores() - 1
#' @param cluster the cluster to use to run the function in parallel
#' @return A list with the row numbers of partitioned data, the rules originally extracted from the model, a list with results from each bootstrap (use stabilitySelection to obtain the stable decison ensemble).
#' @import data.table
#'
#' @example examples/iris_bootstraps.R
#'
#' @export
model2DE_resampling <- function(model, model_type, data, target, classPos = NULL,
                                times = 10, p = .5, sample_weight = NULL,
                                ntree = "all", maxdepth = Inf,
                                dummy_var = NULL,
                                prune = TRUE, maxDecay = 0.05, typeDecay = 2,
                                discretize = TRUE, K = 2, mode = "data",
                                filter = TRUE, min_imp = 0.9
                               , seed = 0, in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL) {


  ### Get the partitions and rules
  tmp <- preCluster(
    model = model, model_type = model_type, data = data, target = target,
    times = times, p = p, sample_weight = sample_weight, classPos = classPos,
    ntree = ntree, maxdepth = maxdepth, dummy_var = dummy_var,
    discretize = discretize, K = K, mode = mode,
    seed = seed,
    in_parallel = in_parallel, n_cores = n_cores
  )
  exec <- tmp$exec
  partitions <- tmp$partitions
  data <- tmp$data


  ### Get a network for each subset
  # define classPos if it has not been passed
  if (is.character(target) && is.null(classPos) == TRUE) {
    classPos <- names(which.max(table(target)))
    cat("Positive class:", classPos, "\n")
  }
  resamp <- list()
  k <- 1
  for (ix in partitions) {
    res <- model2DE(
      data = data[ix, ], target = target[ix],
      exec = exec,
      classPos = classPos,
      prune = prune, maxDecay = maxDecay, typeDecay = typeDecay,
      filter = filter,
      in_parallel = in_parallel, n_cores = n_cores,
      light = TRUE
    )

    # get the decisions: depends on options so we'll just take whatever was produced before getting nodes!
    i_rules <- which(names(res) == "nodes") - 1

    res <- list(
      "pdecisions" = res$n_decisions,
      "rules" = res[[i_rules]],
      "nodes_agg" = res$nodes_agg, "edges_agg" = res$edges_agg
    )

    resamp[[k]] <- res
    k <- k + 1
  }


  return(list("partitions" = partitions, "exec" = exec, "data" = data, "resamp" = resamp))
}
