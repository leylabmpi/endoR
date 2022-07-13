measureSingleDecision <-
  function(ruleExec, data, target, type = "reg", gain = FALSE, pred_null = NULL) {

    # same than original but with the fraction of each class -1 is returned
    # instead of the majority class

    if (gain == FALSE) {
      colNames <- c("len", "support", "err", "condition", "pred")
    } else {
      colNames <- c("len", "support", "err", "condition", "pred", "gain")
    }


    len <- length(unlist(strsplit(ruleExec, split = " & ")))
    origRule <- ruleExec

    ruleExec <- paste("which(", ruleExec, ")")
    ruleExec <- gsub(ruleExec, pattern = "X\\[,", replacement = "data\\[,")
    ixMatch <- eval(parse(text = ruleExec))

    if (length(ixMatch) == 0) {
      v <- rep("-1", length(colNames))
      names(v) <- colNames
      return(v)
    }
    ys <- target[ixMatch]
    pred <- mean(ys, na.rm = TRUE)
    support <- length(ys) / nrow(data)


    if (type == "reg") {
      err <- mean((pred - ys)^2, na.rm = TRUE)
      if (gain == FALSE) {
        v <- c(len, support, err, origRule, pred)
      } else {
        g <- 1 - err / (sum((pred_null - ys)^2) / length(ys))
        v <- c(len, support, err, origRule, pred, g)
      }
    } else {
      if (pred %in% c(0, 1)) {
        err <- 0
      } else {
        err <- 1 - exp(mean(sapply(ys, function(x, pred) {
          x * log(pred) + (1 - x) * log(1 - pred)
        }, pred = pred)))
      }

      if (gain == FALSE) {
        v <- c(len, support, err, origRule, pred)
      } else {
        err_null <- 1 - exp(mean(sapply(ys, function(x, pred) {
          x * log(pred) + (1 - x) * log(1 - pred)
        }, pred = pred_null)))
        g <- 1 - err / err_null
        v <- c(len, support, err, origRule, pred, g)
      }
    }

    names(v) <- colNames
    return(v)
  }




###############################################################

measureAll <-
  function(data, target, classPos = NULL) {
    if (!is.numeric(target)) {
      target <- ifelse(target == classPos, 1, 0)
      pred <- mean(target, na.rm = TRUE)
      err <- 1 - exp(mean(sapply(target, function(x, pred) {
        x * log(pred) + (1 - x) * log(1 - pred)
      }, pred = pred)))
    } else {
      pred <- mean(target, na.rm = TRUE)
      err <- mean((pred - target)^2, na.rm = TRUE)
    }

    v <- c(err, pred)
    names(v) <- c("err", "pred")

    return(v)
  }
