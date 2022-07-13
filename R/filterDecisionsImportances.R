#' Filter decisions according to their metrics
#'
#' This function filters decisions in a heuristic manner according to their importance and multiplicity.
#' A relative importance threshold that maximises the average product relative importance * n and the number of decisions to be removed is calculated.
#' All decisions with a relative importance above that threshold are kept. The argument min_imp is the minimal relative importance of the decisions kept.
#'
#' @param rules data.frame corresponding to the decisions, with all their metrics.
#' @param min_imp minimal relative importance of the decisions that must be kept, the threshold to remove decisions is thus going to take lower values than max(imp)*min_imp.
#' @return The decision ensemble from which decisions with the lowest errors and/or importances have been removed, or are indicated in a column "filt_err"/"filt_imp".
#' @export
filterDecisionsImportances <- function(rules, min_imp = 0.7) {
  if (("data.table" %in% class(rules)) == FALSE) {
    rules <- setDT(rules)
  }

  maxThr <- rules[, max(imp)] * min_imp
  impthr <- seq(0, maxThr, by = maxThr / 500)

  checkImp <- data.table(impThr = impthr)

  for (i in 1:nrow(checkImp)) {
    set(checkImp, i, "nrules", nrow(rules[imp >= impthr[i], ]))
    set(checkImp, i, "sum_impn", sum(rules[imp >= impthr[i], imp * n])) ### remove n
  }

  checkImp <- checkImp[, nremoved := max(nrules) - nrules][, f := sum_impn * nremoved]

  maxD <- max(checkImp[f == max(f), impThr])
  rules <- rules[, filt_imp := "not ok"][imp >= maxD, filt_imp := "ok"]

  message(
    "Threshold for relative importance: < ", round(maxD, digits = 3), " and ",
    nrow(rules[filt_imp == "not ok"]), " rules removed.\n"
  )

  # Filter rules
  rules <- subset(rules, filt_imp == "ok", select = -filt_imp)

  return(rules)
}
