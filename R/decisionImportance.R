#' Compute the importance of decisions
#'
#' This function computes the importance of decisions.
#'
#'
#' @param rules a data.frame with a column "condition" or a vector with name "condition".
#' @param data data from which to get the decision support.
#' @param target response variable.
#' @param classPos if classification, the positive class.
#' @param in_parallel if TRUE, the function is run in parallel.
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use.
#' @param cluster the cluster to use to run the function in parallel.
#'
#' @return the data.frame passed in rules with the gain and importance of the each decision.
#'
#' @export
decisionImportance <- function(rules, data, target, classPos = NULL,
                               in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL) {


  # numerise target
  if (is.numeric(target) == FALSE) {
    target <- ifelse(target == classPos, 1, 0)
    type <- "classification"
  } else {
    type <- "reg"
  }


  if ("data.table" %in% class(data)) {
    data <- as.data.frame(data)
  }

  pred_null <- mean(target, na.rm = TRUE)


  if (in_parallel == TRUE) {
    if (is.null(cluster) == TRUE) {
      message("Initiate parallelisation ... ")
      cluster <- makeCluster(n_cores)
      on.exit(stopCluster(cluster))
    }
    tmp <- parApply(
      cl = cluster, rules, MARGIN = 1, FUN = importanceSingleRule,
      data = data, target = target, type = type, pred_null = pred_null
    )
  } else {
    tmp <- apply(rules,
      MARGIN = 1, FUN = importanceSingleRule,
      data = data, target = target, type = type, pred_null = pred_null
    )
  }


  rules <- rules[, "gain" := tmp][, "imp" := gain * support]

  if ("n" %in% colnames(rules)) {
    rules <- rules[, "n" := as.numeric(n)][order(-n * imp), ]
  } else {
    setorder(rules, -imp)
  }


  return(rules)
}


######################################################
importanceSingleRule <-
  function(rule, data, target, type = "reg", pred_null = NULL) {
    ruleExec <- paste("which(", rule["condition"], ")")
    ruleExec <- gsub(ruleExec, pattern = "X\\[,", replacement = "data\\[,")
    ixMatch <- eval(parse(text = ruleExec))

    if (length(ixMatch) == 0) {
      return(0)
    }
    ys <- target[ixMatch]

    if (type == "reg") {
      err_null <- sum((pred_null - ys)^2) / length(ys)
    } else {
      err_null <- exp(mean(sapply(ys, function(x, pred) {
        x * log(pred) + (1 - x) * log(1 - pred)
      }, pred = pred_null)))
      err_null <- 1 - err_null
    }

    g <- 1 - as.numeric(rule["err"]) / err_null
    return(g)
  }
