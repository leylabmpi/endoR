#' Transform all variables in a decision ensemble into dummy variables.
#'
#' This function first transforms all non-numeric variables from data used to train the original model into dummy variables. It then updates the decision ensemble.
#'
#' @param rules data frame with a 'condition' column.
#' @param data data used to train the original model, or for which the column order corresponds to the column numbers in the rules (column 'condition').
#' @param in_parallel if TRUE, the function is run in parallel (default = FALSE).
#' @param n_cores if in_parallel = TRUE, and no cluster has been passed: number of cores to use, default is detectCores() - 1.
#' @param cluster the cluster to use to run the function in parallel.
#' @export
decisions2FullDummy <- function(rules, data, in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL){

# dummy var
notNum <- which(!sapply(data, is.numeric))
if(length(notNum) == 0){ return(list('rules' = as.data.table(rules), 'data_dummy' = data) ) }



# separate levels in rules
if (in_parallel == TRUE){
    if (is.null(cluster) == TRUE){
          message('Initiate parallelisation ... ')
          cluster <- makeCluster(n_cores)
          clusterEvalQ(cluster, library(endoR))
          clusterEvalQ(cluster, library(stringr))
          on.exit(stopCluster(cluster))
    }

    rules <- t(parApply(cl = cluster,rules, MARGIN = 1, FUN = perlevelRule))
} else {
    rules <- t(apply(rules, MARGIN = 1, FUN = perlevelRule))
}
if(nrow(rules) == 1){
    rules <- t(rules)
    colnames(rules) <- 'condition'
}
if('condition' %in% rownames(rules)){rules <- t(rules)}

for (j in notNum) set(data,i = NULL ,j, paste0('__', data[[j]]))
dummies <- dummyVars( ~ ., data = data)
dummies <- as.data.table(predict(dummies, newdata = data))
for (j in notNum) set(data,i = NULL ,j, str_replace(data[[j]], pattern = '^__', replacement = ''))
colnames(dummies) <- gsub(x = colnames(dummies), pattern = "`", replacement="")


# original column numbers
oriCol <- data.table(var = colnames(data), oldN = 1:ncol(data))
# new column numbers
newCol <- data.table(full = colnames(dummies)
                    , var = str_extract(colnames(dummies), pattern = '.*(?=__)')
                    , lev = str_extract(colnames(dummies), pattern = '(?<=__).*')
                    , newN = 1:ncol(dummies) 
                    )[full %in% oriCol[['var']], `:=`(var=full, lev=NA)]

# merge
newCol <- oriCol[newCol, on = 'var']
#newCol <- merge(newCol, oriCol, all.x = TRUE)
newCol <- newCol[,oldN:=as.character(oldN)][!is.na(lev), oldN:=paste0(oldN, '__', lev)]
setorder(newCol, cols = lev, na.last = FALSE)

mapCol <- paste0("X\\[,", unlist(newCol[,'newN']), "a\\]") ### needs the 'a' trick to avoid replacing multiple times
names(mapCol) <- paste0("X\\[,", unlist(newCol[,'oldN']), "\\]")

# replace variables column numbers
if (in_parallel == TRUE && nrow(rules) > n_cores*5){

    rI <- round(seq(from = 0, to = nrow(rules), length.out = 5*n_cores))
    tmp <- list()
    k <- 1
    for(i in 1:(length(rI)-1)){
        tmp[[k]] <- rules[(rI[i]+1):(rI[i+1]),'condition']
        k <- k+1
    }

    rules[,'condition'] <- unlist(parLapply(cl = cluster, tmp
                                            ,fun = function(X, patVector){str_replace_all(X, patVector)}
                                            , patVector = mapCol))
} else {
    rules[,'condition'] <- str_replace_all(rules[,'condition'], mapCol)
}

rules[,'condition'] <- str_replace_all(rules[,'condition'], pattern = 'a', replacement = '')
    
tmp <- split(as.matrix(rules), seq(nrow(rules)))
tmp <- lapply(tmp, FUN = function(x, newN){names(x) <- newN; return(x)}
              , newN = colnames(rules))

# Make 1 rule per level
if (in_parallel == TRUE){
    rules <- parLapply(cl = cluster, tmp, fun = singleRulePerLevel)
} else {
    rules <- lapply(tmp, FUN = singleRulePerLevel)
}
rules <- do.call(what = rbind,rules)
rules <- unique(as.data.table(rules)[, n:= as.numeric(n)][, n := sum(n), by =  condition])


return(list('rules' = rules, 'data_dummy' = as.data.table(dummies) ) ) 



}


##########################################################

#' @export
perlevelRule <- function(rule){
    # if there isn't any categorical variable, stop directly
    if (str_detect(rule['condition'], pattern = "%in%") == FALSE){return(rule)}

    ruleExec <- unlist(strsplit(rule['condition'], split = ' & '))
    
    # make one condition per level 
    if (str_detect(rule['condition'], pattern = "',") == TRUE){
        for (i in which(sapply(ruleExec, str_detect, pattern = "',"))){
                var <- str_extract(ruleExec[i], pattern = '.*(?= %in% )')
                lev <- unlist(strsplit(str_extract(ruleExec[i], pattern = '(?<=\\().*(?=\\))'), split = ','))
                ruleExec[i] <- paste(paste0(var, " %in% c(",lev,")"), collapse=' | ')
                ruleExec[i] <- paste0("(", ruleExec[i] ,")" )
            }
    }
    for (i in which(sapply(ruleExec, str_detect, pattern = "%in%"))){
        tmp <- unlist(strsplit(ruleExec[i], split = ' \\| '))   
        var <- str_extract(tmp, pattern = '(?<=X\\[,)[:digit:]+')
        lev <- str_extract(tmp, pattern = "(?<=').*(?=')")
        lev <- paste0(var,'__',lev)
        
        for (j in 1:length(var)){
            tmp[j] <- str_replace(tmp[j], pattern = var[j], replacement = lev[j])
            tmp[j] <- str_replace(tmp[j], pattern = " %in% c\\(.*\\'\\){1}(?=\\)?)", replacement = '>0.5')
        }
        tmp <- paste(tmp, collapse = ' | ')
        ruleExec[i] <- tmp
    }
    rule['condition'] <- paste(sort(ruleExec), collapse = ' & ') #yes, yes, we re-order just in case
    
    return(rule)
}


#################################

#' @export
singleRulePerLevel <- function(rule){
    colN <- names(rule)[which(names(rule) != 'condition')]
    
    # if there isn't any categorical variable, stop directly
    if (str_detect(rule['condition'], pattern = "\\|") == FALSE){
        return(c(rule[c(colN, 'condition')], 'to_update' = 'no'))
    }

    ruleExec <- unlist(strsplit(rule['condition'], split = ' & '))
    vars <- str_extract_all(ruleExec, pattern = 'X\\[,[:digit:]+\\](>|<=)-?[:digit:]\\.?[:digit:]*')
    vars <- expand.grid(vars)
    ruleExec <- apply(vars, MARGIN = 1, function(x){paste(sort(x), collapse = ' & ')}) #order here just in case

    newRules <- matrix(rep(rule[which(names(rule) != 'condition')], length(ruleExec)), nrow = length(ruleExec),byrow = T)
    colnames(newRules)<-colN
    newRules <- cbind(newRules, 'condition' = ruleExec)

    if ('n' %in% colN){
        newRules[,'n'] <- as.numeric(newRules[,'n'])/nrow(newRules)
    } else {
        newRules <- cbind(newRules, 'n' =1/nrow(newRules))
    }
    
    return(cbind(newRules, 'to_update' = 'yes'))
}