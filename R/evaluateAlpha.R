#' Calculate the number fo decisions and of predicted samples from decision ensemble obtained with different alpha values
#'
#' The aim is to help picking an alpha that will result in a decision ensemble able to predict most samples. Performs stability selection for each of the given alpha value. The number of decisions and of samples that follow decisions are also calculated. 
#' The procedure is adapted from Meinshausenand and Buehlmann (2010): the best decisions from each bootstrap are pre-seleected and the the ones that were pre-selected in a certain fraction of bootstraps are included in the stable decision ensemble.
#' The decision importances and multiplicities are averaged across bootstraps. Decision-wise feature and interaction importances and influences are averaged across bootstraps before computing the feature and interaction importances and influences from the stable decision ensemble.
#' @param res list of bootstrap results
#' @param alphas expected number of false positive decision selected (default = 1).
#' @param pi_thr fraction of bootstraps in which a decision should have been selected in to be included in the stable decision ensemble (default = 0.7).
#' @return A list with all decisions from all bootstrasps, the summary of decisions across bootstraps, the feature and interaction importance and influence in the nodes and edges dataframes, as well as the the decision-wise feature and interaction importances and influences the nodes_agg and edges_agg dataframes.
#' @export

evaluateAlpha <- function(rules, alphas = c(5, 10, 15, 20, 30, 50, 75), pi_thr = 0.7
			, aggregate_taxa = FALSE, taxa = NULL){

	check_sampl <- data.frame(alpha = numeric(), n_decision = numeric(), n_samples = numeric())

	for (al in ){
	    tmp <- stabilitySelection(res = rules, alpha_error = al, pi_thr = pi_thr, aggregate_taxa = aggregate_taxa, taxa = taxa)
	    tmp <- subset(tmp$rules_summary, inN >= pi_thr*length(rules))
	    
	    cond <- tmp$condition
	    cond <- str_replace_all(cond, pattern = 'X', replacement = 'preclu$data')
	    cond <- paste0('which(', cond, ')')
	    pred_ix <- lapply(cond, function(x){eval(parse(text = x))}) %>% unlist %>% unique 
	    
	    check_sampl <- check_sampl %>% add_row(alpha = al, n_dec = length(cond), n_samp = length(pred_ix))
	}
}