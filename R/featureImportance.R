#' Calculate the feature importances from a decision ensemble
#'
#' This function calculates the featureImportance of variables in a decision ensemble. Levels of discretized variables are grouped so that only one importance is returned ('variable name' separated by 2*'_' from the 'level name').
#'
#' @param nodes_agg a datatable with, for variable in each decision, their name (column Feature), decision-wise importance (column importance), the importance and multiplicity of the decision (columns imp and n).
#'
#' @return a datatable with the feature importance for each variable.
#'
#' @export
featureImportance <- function(nodes_agg){
	
	featImp <- copy(nodes_agg)[, `:=`(Feature = str_extract(var, pattern = '.*(?=\\_{2})') )][
						is.na(Feature), Feature:=var][,.(Feature,importance,imp,n)][
						, importance:= sum(importance*imp*n), by = Feature][,.(Feature,importance)]
	featImp <- unique(featImp)
	setorder(featImp, -importance)
	return(featImp)

}