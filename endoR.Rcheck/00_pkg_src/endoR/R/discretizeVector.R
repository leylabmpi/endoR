# modified from dicretizeVector() of the inTrees package.
discretizeVector <-
  function(v, K = 5, knames = NULL, return_all = FALSE) {

    # set category names
    if (is.null(knames) == TRUE) {
      if (K == 2) {
        knames <- c("min", "Low", "High")
      } else if (K == 3) {
        knames <- c("min", "Low", "Medium", "High")
      } else if (K == 4) {
        knames <- c("min", "veryLow", "Low", "High", "veryHigh")
      } else if (K == 5) {
        knames <- c("min", "veryLow", "Low", "Medium", "High", "veryHigh")
      } else {
        knames <- c("L1", paste("L", seq(1:(K - 1)), sep = ""))
      }
    } else if (length(knames) == K) {
      knames <- c("min", knames)
    }


    splitV <- quantile(v, probs = seq(0, 1, 1 / K), na.rm = TRUE, names = TRUE, type = 3)
    names(splitV) <- knames

    numSplit <- length(splitV)
    if (numSplit < 2) {
      return(v)
    }

    newV <- vector("character", length(v))
    newV[which(v <= splitV[2])] <- names(splitV)[2]

    if (numSplit >= 3) {
      for (jj in 3:numSplit) {
        newV[which(v > splitV[jj - 1] & v <= splitV[jj])] <- names(splitV)[jj]
      }
    }

    splitV <- splitV[c("min", unique(newV))]

    if (return_all == TRUE) {
      return(list("newV" = newV, "splitV" = splitV[order(splitV)]))
    } else {
      return(newV)
    }
  }
