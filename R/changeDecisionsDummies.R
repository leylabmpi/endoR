# takes decisions and modifies them so that only one level of a multiclass variable is used in decisions
changeDecisionsDummies <- function(rules, dummy_var, data, target, classPos = NULL
                               , in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL){

    # get the colnames and column numbers of dummy variables
    dum_lev <- lapply(dummy_var, getDummyLevels, colN = colnames(data))
    names(dum_lev) <- dummy_var
    dum_colN <- unlist(lapply(dummy_var, FUN = dumColnames, colN = colnames(data)))

    # replace the <=0.5 conditions in rules for all other levels with >0.5
    rules <- rules[, to_update:='no']
    # separate levels in rules
    if (in_parallel == TRUE){
        if (is.null(cluster) == TRUE){
            message('Initiate parallelisation ... ')
            cluster <- makeCluster(n_cores)
            clusterEvalQ(cluster, library(babR))
            clusterEvalQ(cluster, library(stringr))
            on.exit(stopCluster(cluster))
        }

        rules <- t(parApply(cl = cluster,rules, MARGIN = 1, FUN = changeSingleRuleDummies
                            , dum_colN = dum_colN, dum_lev = dum_lev ))
    } else {
        rules <- t(apply(rules, MARGIN = 1, FUN = changeSingleRuleDummies
                         , dum_colN = dum_colN, dum_lev = dum_lev ))
    }


    # get one rule per level
    tmp <- split(rules, seq(nrow(rules)))
    tmp <- lapply(tmp, FUN = function(x, newN){names(x) <- newN; return(x)}
                  , newN = colnames(rules))
    if (in_parallel == TRUE){
        rules <- parLapply(cl = cluster, tmp, fun = singleRulePerLevel)
    } else {
        rules <- lapply(tmp, FUN = singleRulePerLevel)
    }

    rules <- do.call(what = rbind,rules)
    rules <- as.data.table(rules)[, n:= as.numeric(n)][, n := sum(n), by =  condition]

    if ('err' %in% colnames(rules)){
        rules <- rules[to_update == 'yes', to_up := 'yes'][, to_update:=NULL][, to_update:=NULL]
        rules <- unique(rules)
        # Get the new metrics
        if (nrow(rules[to_up == 'yes',]) > 0 ){
            if ('imp' %in% colnames(rules)){
                importances <- TRUE
                colN <- c('len','err','support','pred', 'imp', 'to_up')
            } else {
                importances <- FALSE
                colN <- c('len','err','support','pred' ,'to_up')
            }

            no_up <- unique(subset(rules, is.na(to_up), select = -to_up))
            tmp1 <- unique(subset(rules, to_up == 'yes' & !(condition %in% no_up$condition))[, (colN):=NULL])

            tmp2 <-  getDecisionsMetrics(tmp1
                                  , data = data, target = target, classPos = classPos
                                  , importances = importances
                                  , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
            tmp2 <- tmp1[tmp2, on = 'condition']
            #tmp2 <- merge(tmp2, tmp1, all.x = TRUE, by = 'condition' )

            if (nrow(no_up) > 0 ){
                rules <- rbind(no_up, tmp2)
            } else {
                rules <- tmp2
            }

            rules <- rules[, c('len','support','err','pred', 'n'):=lapply(.SD, as.numeric)
                                           , .SDcols = c('len','support','err','pred', 'n')]
            rules <- unique(rules[, n := sum(n), by =  condition])

        }
    } else { rules <- unique(rules[, to_update:=NULL][, to_update:=NULL]) }


    return(rules)
}


##################################################
getDummyLevels <- function(var, colN){
    sub <- str_which(colN, pattern = paste0('^', var))
    sub <- paste0('X[,', sub, ']<=0.5')
    return(sub)
}
dumColnames <- function(var, colN){
    sub <- str_which(colN, pattern = paste0('^', var))
    dum <- rep(var, length(sub))
    names(dum) <- paste0('X[,', sub, ']<=0.5')
    return(dum)
}

#################################################
changeSingleRuleDummies <- function(rule, dum_colN, dum_lev){

    ruleV <- unlist(strsplit(rule['condition'], ' & '))
    if(length(ruleV[ruleV %in% names(dum_colN)]) > 0){
        oriRule <- ruleV[!(ruleV %in% names(dum_colN))]
        ruleV <- ruleV[ruleV %in% names(dum_colN)]
        names(ruleV) <- dum_colN[ruleV]
        vdum <- unique(names(ruleV))
        newV <- c()
        for (v in vdum){
            tmp <- names(dum_colN[dum_colN == v])
            tmp <- tmp[!(tmp %in% ruleV)]
            tmp <- str_replace(tmp, pattern = '<=', replacement = '>')
            if (length(tmp[tmp %in% oriRule]) > 0) next
            if (length(tmp) > 1){ tmp <- paste0('(', paste(tmp, collapse = ' | '), ')') }
            newV <- c(newV, tmp)
        }
    rule['condition'] <- paste0(sort(c(oriRule, newV)), collapse = ' & ')
    rule['to_update'] <- 'yes'
    }

    return(rule)
}
