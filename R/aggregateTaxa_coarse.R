aggregateTaxa_coarse <- function(taxa, weights, thr = NULL) {
  ### taxa: the elongated features (from elongateTaxa), the longFeature column is not necessary
  ### weights: df with the features and the weights only (the highest the best)

  if (is.null(thr) == TRUE) {
    thr <- mean(weights$weight)
  }

  # some formatting
  if (!("data.table" %in% class(taxa))) {
    taxa <- as.data.table(taxa)[, Feature := as.character(Feature)]
  }
  taxa <- merge(taxa, weights[, list(Feature, weight)], by = "Feature", all.x = TRUE)
  setorder(taxa, weight)
  taxa <- taxa[!is.na(f), ][, `:=`(newFeature = Feature, newWeight = weight, ix = .I)]
  tax_col <- which(colnames(taxa) %in% c("f", "g", "s"))
  lim <- taxa[weight > thr, min(ix)]


  for (i in 1:lim) {

    # subset to the features of the same family doing better (to look at the right spot)
    tmp <- taxa$f[i]
    to_check <- taxa[f == tmp, ix]
    to_check <- to_check[which(to_check > i)]

    if (length(to_check) > 0) {

      # record if features have the same genus/family names and how good is their rank
      check <- matrix(ncol = 4)
      colnames(check) <- c(colnames(taxa)[tax_col], "ix")
      for (k in 1:length(to_check)) {
        tmp <- cbind((taxa[i, c(tax_col), with = FALSE] == taxa[to_check[k], c(tax_col), with = FALSE]),
          ix = to_check[k]
        )
        check <- rbind(check, tmp)
      }
      check <- as.data.table(check, stringsAsFactors = FALSE)

      # select the one with the thinnest taxonomic level
      if (is.na(taxa$s[i]) == FALSE) {
        better <- check[is.na(s) & g == 1, ix]
      } else if (is.na(taxa$g[i]) == FALSE) {
        better <- check[is.na(g) & f == 1, ix]
      } else {
        better <- i
      }

      # and give it to the bad one
      if (length(better) > 0) {
        # to make it retro-active!
        to_change <- taxa[newFeature == taxa[["Feature"]][i], ix]
        taxa <- taxa[ix %in% to_change, `:=`(
          newFeature = taxa[["newFeature"]][max(better)],
          newWeight = taxa[["newWeight"]][max(better)]
        )]
      }
    }
  }

  taxa <- taxa[, "n" := .N, by = newFeature][, "changed" := "Unchanged"][
    n > 1 & newFeature == Feature, changed := "Recipient"
  ][
    n > 1 & newFeature != Feature, changed := "Downgraded"
  ][
    , changed := as.factor(changed)
  ]

  return(taxa)
}
