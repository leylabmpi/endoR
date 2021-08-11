model2DE_resamp11 <- function(
      model, model_type, data, target, classPos = NULL
      , times = 10, p = .5, sample_weight = NULL
      , ntree = 'all', maxdepth = Inf
      , dummy_var = NULL
      , prune = TRUE, maxDecay = 0.05, typeDecay = 2
      , discretize = TRUE, K = 2, features_ctg = NULL
      , filter = TRUE, min_imp = 0.9
      , seed = 0, in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL
      ){


### Get the partitions and rules
tmp <- preCluster(model = model, model_type=model_type, data=data, target=target
                  , times = times, p = p, sample_weight = sample_weight, classPos = classPos
                  , ntree = ntree, maxdepth = maxdepth, dummy_var = dummy_var
                  , discretize = discretize, K = K, features_ctg = features_ctg
                  , seed = seed
                  , in_parallel = in_parallel, n_cores = n_cores)
exec <- tmp$exec
partitions <- tmp$partitions
data <- tmp$data


### Get a network for each subset
# define classPos if it has not been passed
if (is.character(target) && is.null(classPos) == TRUE) {
    classPos <- names(which.max(table(target)))
    cat('Positive class:', classPos, '\n')
}
resamp <- list()
k <- 1
for (ix in partitions){
    res <- model2DE(data = data[ix, ], target = target[ix]
                      , exec = exec
                      , classPos = classPos
                      , prune = prune, maxDecay = maxDecay, typeDecay = typeDecay
                      , filter = filter
                      , in_parallel = in_parallel, n_cores = n_cores
                      , light = TRUE)
    #res <- list('pdecisions' = res$n_decisions
    #            , 'rules' = res[[length(res)-4]]
    #            , 'nodes_agg' = res$nodes_agg, 'edges_agg' = res$edges_agg)
    resamp[[k]] <- res
    k <- k+1
}


return(list('partitions' = partitions, 'exec' = exec, 'resamp' = resamp))

}
