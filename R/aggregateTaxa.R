#' Aggregate taxa according to a measure.
#'
#' This function aggregates taxa according to their rank from a measure passed in weights: if a taxa has a lower rank than its coarser level, type = 'coarse', it is aggregated into the coarser; if their is a unique thiner level with a better rank than the coarser one, then the coarser is aggregated into the thiner Comparison is done only for the family, genus and specie levels.
#'
#' @param taxa taxa should be a vector with the long format of the features of the taxa table with taxonomic levels as columns - cf taxa2vector; or a data.frame with the feature name and all its other coarser/thiner levels.
#' @param features if taxa is a vector or a data.frame that lacks all levels, a vector of the features to be checked.
#' @param weights a data.frame with a column Feature and a column weight; for type = 'both' or 'coarse' only.
#' @param thr numeric, value after which the algorithm should stop looking for better ranks in a taxa banch; for type = 'both' or 'coarse' only. If NULL, the median of weights is used.
#' @param type character. If 'coarse', finer levels are aggregated into their coarser one if it has a better rank. If 'fine' then coarser levels are aggregated into a finer level if it has a better rank and is unique, ie there is a unique finer level for that coarser level. If 'both', both aggregation steps are seuqentially performed.
#'
#' @return A dataframe with aggregated features in the "Feature" column, and the 'recipient' taxa in the "newFeature" column.
#'
#' @export
aggregateTaxa <- function(taxa, features = NULL, weights = NULL, thr = NULL, type = "coarse") {
  if (!is.null(weights) && !("weight" %in% colnames(weights))) {
    wI <- which(sapply(weights, is.numeric))
    colnames(weights)[wI] <- "weight"
  }

  if (is.vector(taxa) == TRUE || !("Feature" %in% colnames(taxa))) {
    taxa <- elongateTaxa(features = features, taxa = taxa)
  }

  if (is.null(thr) == TRUE) {
    thr <- median(weights$weight)
  }

  ### wrapper of the 2 collapsing functions, with the option of performing both
  if (type == "coarse") {
    taxa <- aggregateTaxa_coarse(taxa, weights, thr)
  }

  if (type == "fine") {
    taxa <- aggregateTaxa_fine(taxa)
  }

  if (type == "both") {
    taxa_d <- aggregateTaxa_coarse(taxa, weights, thr)
    taxa_u <- aggregateTaxa_fine(subset(taxa, Feature %in% taxa_d$newFeature))

    taxa <- unique(taxa_u[, .(Feature, newFeature)])[, "coarseFeature" = newFeature][, newFeature := NULL]
    taxa <- merge(taxa, taxa_d[, .(newFeature, Feature, weight, newWeight)], by = c("coarseFeature" = "newFeature"))

    # record how many times a newFeature has been attributed and who was changed
    taxa <- taxa[, n := .N, by = "newFeature"][, "changed" := "Unchanged"][
      n > 1 & newFeature == Feature, changed := "Recipient"
    ][
      newFeature != coarseFeature & coarseFeature == Feature, changed := "Upgraded"
    ][
      newFeature == coarseFeature & coarseFeature != Feature, changed := "Downgraded"
    ][
      newFeature != coarseFeature & coarseFeature != Feature, changed := "Down&Upgraded"
    ][
      , changed := as.factor(changed)
    ]
  }

  return(taxa)
}
