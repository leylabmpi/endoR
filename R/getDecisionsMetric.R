#' Measure the error, prediction and importance of decisions
#'
#' This function measures the prediction and error on the response variable of each decision on its support in the data passed. The importance is calculated by default but this can be switched off.
#'
#' @param ruleExec  a vector with name "condition" or a data.frame with a column "condition".
#' @param data data from which to get the decision support.
#' @param target response variable.
#' @param classPos for clssification tasks, the positive class to be predicted by decisions.
#' @param importances if FALSE, the importances are not calculated (importances = TRUE by default).
#' @param in_parallel if TRUE, the function is run in parallel.
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use.
#' @param cluster the cluster to use to run the function in parallel.
#' @return a datatable with the rule (column "condition"), error ("err"), prediction ("pred") support, number of variables in the decision rule ("len"). Columns "gain" and "imp" wit hthe gain and importance of teh decision are added if importances were calculated.
#'
#' @example examples/iris_each_function.R
#'
#' @export
getDecisionsMetrics <- function(ruleExec, data, target, classPos = NULL,
                                importances = TRUE,
                                in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL) {
  # modified from getRuleMetrics in the inTrees R-package

  # get the classPos
  if (is.numeric(target) == FALSE) {
    if (is.null(classPos) == TRUE) {
      classPos <- names(which.max(table(target)))
      cat("Positive class:", classPos, "\n")
    }
    target <- ifelse(target == classPos, 1, 0)
    type <- "classification"
  } else {
    type <- "reg"
  }

  if ("data.table" %in% class(data)) {
    data <- as.data.frame(data)
  }

  if (importances == TRUE) {
    pred_null <- mean(target, na.rm = TRUE)
  } else {
    pred_null <- NULL
  }



  if (in_parallel == FALSE) {
    ruleMetric <- t(sapply(as.matrix(ruleExec[, "condition"]), measureSingleDecision,
      data = data, target = target, type = type,
      gain = importances, pred_null = pred_null
    ))
  } else {
    if (is.null(cluster) == TRUE) {
      message("Initiate parallelisation ... ")
      cluster <- makeCluster(n_cores)
      clusterEvalQ(cluster, library(inTrees))
      on.exit(stopCluster(cluster))
    }

    message("Calculate metrics ... ")
    ruleMetric <- parLapply(cluster, as.matrix(ruleExec[, "condition"]), measureSingleDecision,
      data = data, target = target, type = type,
      gain = importances, pred_null = pred_null
    )

    if (importances == FALSE) {
      colN <- c("len", "support", "err", "condition", "pred")
    } else {
      colN <- c("len", "support", "err", "condition", "pred", "gain")
    }

    ruleMetric <- matrix(unlist(ruleMetric), nrow = length(colN), byrow = FALSE)
    rownames(ruleMetric) <- colN
    ruleMetric <- t(ruleMetric)
  }

  rownames(ruleMetric) <- NULL
  # remove the decisions with empty support
  dIx <- which(ruleMetric[, "len"] == "-1")
  if (length(dIx) > 0) {
    ruleMetric <- ruleMetric[-dIx, ]
  }
  if ("character" %in% class(ruleMetric)) {
    ruleMetric <- t(ruleMetric)
  }


  if (importances == TRUE) {
    ruleMetric <- as.data.table(ruleMetric)[, c("len", "support", "err", "pred", "gain") := lapply(.SD, as.numeric), .SDcols = c("len", "support", "err", "pred", "gain")][
      , "imp" := gain * support
    ]
  } else {
    ruleMetric <- as.data.table(ruleMetric)[, c("len", "support", "err", "pred") := lapply(.SD, as.numeric), .SDcols = c("len", "support", "err", "pred")]
  }


  return(ruleMetric)
}
