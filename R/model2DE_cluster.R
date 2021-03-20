#' Run model2DE on several bootstrap resamples in parallel.
#'
#' Function to with the Q() function from the clustermq R-package with the following arguments in export: 
#' data, target, exec, classPos, dummy_var, prune, maxDecay, typeDecay, filter, in_parallel, n_cores.
#' See preCluster() to obtain the list of boostraps resamples, the discretized data and exec dataframe with decisions.
#' 
#' @param partition a vector with row numbers to subset data.
#' @export
model2DE_cluster <- function(partition){
    library(data.table)
    res <- model2DE(data = data[partition, ], target = target[partition]
                      , exec = exec
                      , classPos = classPos, dummy_var = dummy_var
                      , prune = prune, maxDecay = maxDecay, typeDecay = typeDecay 
                      , filter = filter, min_imp = 1
                      , in_parallel = in_parallel, n_cores = n_cores
                      , light = TRUE)
    res <- list('pdecisions' = res$n_decisions
                , 'rules' = res[[length(res)-4]]
                , 'nodes_agg' = res$nodes_agg, 'edges_agg' = res$edges_agg)
    return(res)
}