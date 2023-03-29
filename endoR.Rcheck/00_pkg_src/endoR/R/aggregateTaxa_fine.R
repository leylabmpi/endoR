aggregateTaxa_fine <- function(taxa) {
  ### taxa: the elongated features (from elongateTaxa), the longFeature column is not necessary

  if (!("data.table" %in% class(taxa))) {
    taxa <- as.data.table(taxa)[, Feature := as.character(Feature)]
  }
  # create the columns for the updating
  taxa <- taxa[, `:=`(newFeature = Feature, ix = .I)]
  tax_col <- which(colnames(taxa) %in% c("f", "g", "s"))

  ### First: at the genus level
  not_s <- taxa[is.na(s) & !is.na(g), ix]
  for (i in not_s) {
    # subset the specie of the same genus
    tmp <- taxa[["g"]][i]
    fine <- taxa[g == tmp & !is.na(s), newFeature]
    if (length(fine) == 1) {
      taxa <- taxa[ix == i, newFeature := fine]
    }
    fine <- NULL
  }

  ### Then: at the family level
  not_s <- taxa[is.na(g), ix]
  for (i in not_s) {
    # subset to the features of the same family
    tmp <- taxa[["f"]][i]
    fine <- unique(taxa[f == tmp & !is.na(g), newFeature])
    if (length(fine) == 1) {
      taxa <- taxa[ix == i, newFeature := fine]
    }
    fine <- NULL
  }

  # record how many times a newFeature has been attributed and who was changed
  taxa <- taxa[, "n" := .N, by = newFeature][, "changed" := "Unchanged"][
    n > 1 & newFeature == Feature, changed := "Recipient"
  ][
    n > 1 & newFeature != Feature, changed := "Upgraded"
  ][
    , changed := as.factor(changed)
  ]

  return(taxa)
}
