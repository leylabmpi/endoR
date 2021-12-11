#' Obtain a stable decision ensemble
#'
#' Performs stability selection after bootstrapping with the model2DE_cluster or model2DE_resampling functions. 
#' The procedure is adapted from Meinshausenand and Buehlmann (2010): the best decisions from each bootstrap are pre-seleected and the the ones that were pre-selected in a certain fraction of bootstraps are included in the stable decision ensemble.
#' The decision importances and multiplicities are averaged across bootstraps. Decision-wise feature and interaction importances and influences are averaged across bootstraps before computing the feature and interaction importances and influences from the stable decision ensemble.
#' @param rules list of bootstrap results
#' @param alpha_error expected number of false positive decision selected (default = 1).
#' @param pi_thr fraction of bootstraps in which a decision should have been selected in to be included in the stable decision ensemble (default = 0.7).
#' @param aggregate_taxa should taxa be aggregated at the genus level (if species have lower importance than their genus) or species level (if a genus is represented by a unique species)
#' @param taxa if aggregate_taxa = TRUE, a data.frame with all taxa included in the dataset: columns = taxonomic ranks (with columns f, g, and s)
#' @return A list with all decisions from all bootstrasps, the summary of decisions across bootstraps, the feature and interaction importance and influence in the nodes and edges dataframes, as well as the the decision-wise feature and interaction importances and influences the nodes_agg and edges_agg dataframes.
#' @export
stabilitySelection <- function(rules, alpha_error = 1, pi_thr = 0.7, aggregate_taxa = FALSE, taxa = NULL){

  agg_res <- list()
  on.exit(return(agg_res), add = TRUE, after = TRUE)

  # if endoR has not been ran on the cluster, the bootstraps are in 'resamp'
  if ('resamp' %in% names(rules)){rules <- rules$resamp}
    
  # Get the number of rules to select per subset
  # average number of decisions per subset
  pall <- mean(sapply(rules, function(x){return(x$pdecisions)} ))
  qsubset <- sqrt((2*pi_thr-1)*alpha_error*pall)
  if (qsubset < 1){qsubset <- 1} # if less than a decision should be selected, take at least one
  cat(qsubset, ' rules per sub-sample selected. ')
  # save the parameters for later
  agg_res$parameters <- c('pi_thr' = length(rules)*pi_thr, 'alpha_error' = alpha_error, 'q' = qsubset)

  # Create a data.frame with intersection of rules
  rules_agg <- lapply(rules, function(x, qsubset){setorder(x$rules[,wimp:=imp*n], -wimp)[,wimp:=NULL];return(x$rules[1:min(qsubset, nrow(x$rules)),])}, qsubset=qsubset )
  rules_agg <- do.call(what = rbind, rules_agg)
  rules_agg <- rules_agg[,.(len,support,err,condition, pred, imp, n)]
  
  rules_agg[is.na(rules_agg)] <- 0
  agg_res$rules_agg <- rules_agg


  # add summary rules
  rules_summary <- unique(copy(rules_agg)[, .(condition,imp,n, len, support, err, pred)][
                                , .(inN = .N, len=mean(len), support = mean(support), err = mean(err), pred = mean(pred)
                                , imp=mean(imp), imp_sd=sd(imp)
                                , n=mean(n), n_sd=sd(n) ), by = condition])[
                              is.na(imp_sd), imp_sd := 0 ][
                              is.na(n_sd), n_sd := 0 ]
  setorder(rules_summary, -inN, -imp)
  agg_res$rules_summary <- rules_summary

  # subset decisions to get the most stable ones
  final_decisions <- rules_summary[inN >= length(rules)*pi_thr, (condition)]
  if (length(final_decisions) == 0){
    warning("No stable decision ensemble could be reached: try increasing alpha.")
    return(agg_res)
  }
  cat(nrow(subset(rules_summary, inN >= length(rules)*pi_thr)), 'decisions in >=', length(rules)*pi_thr, 'subsets.\n')


  # get all edges
  edges_agg <- lapply(rules, function(x){return(x$edges_agg)} )
  edges_agg <- do.call(what = rbind, edges_agg)
  agg_res$edges_agg <- copy(edges_agg)
  
  # get all nodes
  nodes_agg <- lapply(rules, function(x){return(x$nodes_agg)} )
  nodes_agg <- do.call(what = rbind, nodes_agg)
  agg_res$nodes_agg <- copy(nodes_agg)

  nodes <- subset(nodes_agg, condition %in% final_decisions
                  , select = c(var, condition, importance, influence, d))
  edges <- subset(edges_agg, condition %in% final_decisions
                  , select = c(x,y, condition, importance, influence, d.x, d.y))


  ## if aggregate_taxa = TRUE, get the new taxa names
  ## Get the feature importance
  if (aggregate_taxa == TRUE){
    featImp <- copy(nodes)

    # replace variables column numbers
    featImp <- unique(featImp[, 'Feature' := str_extract(var, pattern = '.*(?=\\_{2})') ][
                    is.na(Feature), 'Feature':=var][,.(Feature, importance)][
                    , importance:= sum(importance), by = Feature])
    setorder(featImp, -importance)
    agg_res$featImp <- featImp

    newFeatures <- aggregateTaxa(taxa = taxa,features = featImp$Feature, weights = featImp)
    if ('Recipient' %in% newFeatures$changed){
      tmp <- as.data.table(newFeatures)[,.(Feature, newFeature)][newFeature != Feature,]
      mapCol <- unlist(tmp[,'newFeature'])
      names(mapCol) <- paste0("^", unlist(tmp[,'Feature']), "(?=(\\_{2}.*)|$)")
      nodes$var <- str_replace_all(nodes$var, mapCol)
      edges$x <- str_replace_all(edges$x, mapCol)
      edges$y <- str_replace_all(edges$y, mapCol)
      edges <- edges[,'change':=(x>y)][change == TRUE,`:=`(x=y,y=x)][,change:=NULL]
    }
    agg_res$newFeatures <- newFeatures
  }


  # summary nodes
  nodes <- nodes[importance < 0, importance:=0][, .(importance= mean(importance), influence = mean(influence*d))
                 , by = c('var', 'condition')]
  nodes <- merge(nodes, rules_summary[,.(condition, imp, n, len)], all.x = TRUE, by = 'condition')[
               , .(importance = sum(importance*imp*n)
                   , influence = sum(influence*imp*n)/sum(imp*n) ), by = var]
  
  # summary edges
  edges <- edges[is.na(importance), importance:=0][, .( importance= mean(importance), influence = mean(influence*(d.x+d.y)/2)
                     , association_sign = mean(d.x*d.y) )
                 , by = c('x', 'y', 'condition')]
  edges <- merge(edges, rules_summary[,.(condition, imp, n, len)], all.x = TRUE, by = 'condition')[
               , .(importance = sum(importance*imp*n)
                   , influence = sum(influence*imp*n)/sum(imp*n)
                   , association_sign = sum(association_sign*imp*n)/sum(imp*n) ) 
                   , by = c('x', 'y')][,'d_assoc':= as.character(sign(association_sign))]
                   

  agg_res$nodes <- nodes 
  agg_res$edges <- edges 
    
  return(agg_res)
}