#' Transform a decision ensemble into a network
#'
#' Takes a decision ensemble and measures the importance and influence of each feature and pair of features to create a network. 
#' For categorical variables or discretized ones, the importance and influence are calculated per level. See featureImportance to obtain the overall feature importance.
#'
#' @param rules the decision ensemble.
#' @param data data from which to measure characteristic.
#' @param target response variable.
#' @param classPos the positive class predicted by decisions.
#' @param in_parallel if TRUE, the function is run in parallel.
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use.
#' @param cluster the cluster to use to run the function in parallel.
#' @return A list with in the nodes and edges dataframes, the feature and interaction importance and influence; the decision-wise feature and interaction importances and influences are contained in the nodes_agg and edges_agg dataframes.
#' @export
getNetwork <- function(rules, data, target, classPos = NULL
                      , additional_decisions = NULL
                      , aggregate_taxa = FALSE, taxa = NULL, type = 'coarse'
                      , in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL){



if (in_parallel == TRUE){
  if (is.null(cluster) == TRUE){
    message('Initiate parallelisation ... ')
    cluster <- makeCluster(n_cores)
    clusterEvalQ(cluster, library(stringr))
    on.exit(stopCluster(cluster))
  }
}
res <- list()
on.exit(return(res), add = TRUE, after = TRUE)


## Get the character names of the features
featNames <- data.table(varN = colnames(data)
                       , var = as.character(1:ncol(data)) )
res$featNames <- featNames

## Get the additional decision metrics tables
if (is.null(additional_decisions)){
  rulesAdd <- getComplements(rules = rules, data = data, target = target, classPos = classPos
                  , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster )
} else {rulesAdd <- additional_decisions}
res$rulesAdd <- rulesAdd

if (length(unique(target)) <= 2){
  target <- ifelse(target == classPos, 1, 0)
  type <- 'classification'
} else{type <- 'reg'}


## Get nodes - part 1
nodes <- copy(rulesAdd$rm)[,.(ruleID,var, pred, support)]
setnames(nodes, old = 'pred', new = 'pred.rm')
setnames(nodes, old = 'support', new = 's.rm')
nodes <- merge(nodes, copy(rulesAdd$original)[,.(ruleID, condition, support, err, pred, imp, n)]
            , by = 'ruleID', all.x = TRUE )

if (in_parallel == TRUE){
  tmp <- parApply(cl = cluster, nodes, MARGIN = 1, FUN = rmError, data = data
                  , target = target, type=type)
} else {
  tmp <- apply(nodes, MARGIN = 1, FUN = rmError, data = data
               , target = target, type = type)
}
nodes$err.rm <- tmp

nodes <- merge(nodes, copy(rulesAdd$directions)[,.(ruleID, var, d)]
            , by = c('ruleID', 'var'), all.x = TRUE )
nodes <- nodes[,'importance' := round(err.rm, digits = 10)-round(err, digits = 10)][
    , 'influence' := d*(pred-pred.rm)][
    is.na(importance), importance:=0][is.na(influence), influence:=0]


## if aggregate_taxa = TRUE, get the new taxa names
## Get the feature importance
if (aggregate_taxa == TRUE){
featImp <- copy(nodes)

# replace variables column numbers
featImp <- featNames[ featImp, on =  'var'][,var:=NULL]
setnames(featImp, old = 'varN', new = 'var')
featImp <- unique(featImp[, 'Feature' := str_extract(var, pattern = '.*(?=\\_{2})') ][
                  is.na(Feature), 'Feature':=var][,.(Feature, importance)][
                  , importance:= sum(importance), by = Feature])
setorder(featImp, -importance)
res$featImp <- featImp

newFeatures <- aggregateTaxa(taxa = taxa,features = featImp$Feature, weights = featImp)
if ('Recipient' %in% newFeatures$changed){
    tmp <- as.data.table(newFeatures)[,.(Feature, newFeature)][newFeature != Feature,]
    mapCol <- unlist(tmp[,'newFeature'])
    names(mapCol) <- paste0("^", unlist(tmp[,'Feature']), "(?=(\\_{2}.*)|$)")
    featNames$varN <- str_replace_all(featNames$varN, mapCol)
}
res$featNames <- featNames
res$newFeatures <- newFeatures
}


## edges - part 1
tmp <- split(as.matrix(rulesAdd$original), seq(nrow(rulesAdd$original)))
tmp <- lapply(tmp, FUN = function(x, newN){names(x) <- newN; return(x)}
              , newN = colnames(rulesAdd$original))
edges <- lapply(tmp, interactionVariables)
edges <- as.data.table(do.call(what = rbind, edges))
setnames(edges, c('x', 'y', 'ruleID'))
edges <- subset(edges, !is.na(x))
nodes$fullID <- paste0(nodes$ruleID,'_', nodes$var)
edges$fullID.x <- paste0(edges$ruleID,'_', edges$x)
edges$fullID.y <- paste0(edges$ruleID,'_', edges$y)
edges <- merge(edges, unique(select(nodes, c(ruleID, condition, pred, err, imp, n, support))), all.x = TRUE, by = 'ruleID')
edges <- merge(edges, select(nodes, c(fullID, importance, pred.rm, s.rm, d))
                   , by.x = 'fullID.x', by.y = 'fullID', all.x = TRUE)
setnames(edges, old = 'importance', new = 'importance.x')
setnames(edges, old = 'pred.rm', new = 'predRm.x')
setnames(edges, old = 's.rm', new = 'sRm.x')
setnames(edges, old = 'd', new = 'd.x')
edges <- merge(edges, select(nodes, c(fullID, importance, pred.rm, s.rm, d))
                   , by.x = 'fullID.y', by.y = 'fullID', all.x = TRUE)
setnames(edges, old = 'importance', new = 'importance.y')
setnames(edges, old = 'pred.rm', new = 'predRm.y')
setnames(edges, old = 's.rm', new = 'sRm.y')
setnames(edges, old = 'd', new = 'd.y')
edges <- edges[, 'predRm.xy' := (predRm.x*sRm.x + predRm.y*sRm.y - pred*support)/(sRm.x+sRm.y-support)][
               , `:=`('importance'=sqrt(importance.x*importance.y)
                      , 'influence'=(pred- predRm.xy)*(d.x+d.y)/2 )][
                         , c('fullID.x', 'fullID.y'):=NULL]


## Get nodes - part 2
# replace variables column numbers
nodes <- featNames[ nodes, on =  'var'][,var:=NULL]
setnames(nodes, old = 'varN', new = 'var')

# Get the average per variable
nodes_mean <- copy(nodes)[importance < 0, importance:=0][, .( importance = sum(importance*imp*n)
                              , wParticipation = sum(importance*imp*n)/sum(imp*n)
                              , influence = sum(influence*imp*n)/sum(imp*n)
                             )
                          , by = 'var']
res$nodes_agg <- nodes
res$nodes <- nodes_mean


# Edges - part 2
edges <- edges[,'change':=(x>y)][change == TRUE,`:=`(x=y,y=x)][,change:=NULL]
edges <- featNames[edges, on = c('var' = 'x')][,var:=NULL]
setnames(edges, old = 'varN', new = 'x')
edges <- featNames[edges, on = c('var' = 'y')][,var:=NULL]
setnames(edges, old = 'varN', new = 'y')


edges_mean <- copy(edges)[is.na(importance), importance:=0][, .(importance = sum(importance*imp*n)
                              , wParticipation = sum(importance*imp*n)/sum(imp*n)
                              , influence = sum(influence*imp*n)/sum(imp*n)
                              , association_sign = sum(d.x*d.y*imp*n)/sum(imp*n) )
                              , by = c('x', 'y')][,'d_assoc':= as.character(sign(association_sign))]
res$edges <- unique(edges_mean)
res$edges_agg <- edges


return(res)

}




##########################################################

#' @export
### Loop the variable in decisions
interactionVariables <- function(rule){
  var <- unlist(str_extract_all(rule['condition'], pattern = '(?<=X\\[,)[:digit:]+(?=\\])'))
  if (length(var) > 1){
    var <- t(combn(var,2))
    var <- cbind(var, rep(rule['ruleID'], nrow(var)))
    return(var)
  } else {
    return( c(NA, NA,rule['ruleID']) )
  }

}

##########################################################


#' @export
rmError <- function(rule, data, target, type = 'reg'){

ruleExec <- paste("which(", rule['condition'], ")")
ruleExec <- gsub(ruleExec, pattern = "X\\[,", replacement = "data\\[,")
ixMatch <- eval(parse(text=ruleExec))

if(length(ixMatch)==0){ return(0) }

pred <- as.numeric(rule['pred.rm'])
if (type == 'reg') {
  ys <- target[ixMatch]
  return(sum((pred - ys)^2))
} else {
  ys <- target[ixMatch]
  if (pred %in% c(0,1)){
      err <- 0
  } else {
      err <- 1-exp(mean( sapply(ys, function(x,pred){x*log(pred)+(1-x)*log(1-pred)}, pred = pred) ))
  }
  return(err)
}
}
