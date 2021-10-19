getComplements <- function(rules, data, target, classPos = NULL
							, in_parallel = FALSE, n_cores=detectCores() - 1, cluster = NULL){

	if (!('data.table' %in% class(rules))){rules <- as.data.table(rules)}

	# give rules ID
	rules <-rules[,ruleID:=paste0('decision',.I)]


	# get the single complements and rm rules
	if (in_parallel == FALSE){
		tmp <- apply(rules, MARGIN = 1, complementSingleRule )
	} else {
		if (is.null(cluster) == TRUE){
	      message('Initiate parallelisation ... ')
	      cluster <- makeCluster(n_cores)
	      clusterEvalQ(cluster, library(stringr))
	      on.exit(stopCluster(cluster))
	    }

	    message('Generate additional decisions ... ')
	    tmp <- parApply( cl = cluster, rules, MARGIN = 1, FUN = complementSingleRule )
	}
	# the rm decisions
  	rulesAdd <- unlist(lapply(tmp, function(x){x$nR}))
  	names(rulesAdd) <- str_replace(names(rulesAdd), pattern = '^decision[:digit:]+\\.', replacement = '')
  	# the signs
  	sVar <- unlist(lapply(tmp, function(x){x$s}))
  	names(sVar) <- str_replace(names(sVar), pattern = '^decision[:digit:]+\\.', replacement = '')
  	sVar <- unlist( lapply( sVar, FUN = function(x){eval(parse(text=x))} ) )


	# get the directions
	directions <- data.table( ruleID = str_extract(names(sVar), pattern = '^decision[:digit:]+(?=var)')
						, var = str_extract(names(sVar), pattern = '(?<=var)[:digit:]+$')
						, d = ifelse(sVar == TRUE, 1, -1) )


	# get metrics for the rm
	cIx <- str_which(names(rulesAdd), pattern = 'varRm')
	rulesRm <- data.table(fullID = names(rulesAdd)[cIx]
						, ruleID = str_extract(names(rulesAdd)[cIx], pattern = '^decision[:digit:]+(?=var)')
						, var = str_extract(names(rulesAdd)[cIx], pattern = '(?<=varRm)[:digit:]+$')
						, condition = rulesAdd[cIx])
	tmp <- unique(copy(rulesRm)[condition != '', .(condition)]) # remove repetitions due to the dummy variables
	if (nrow(tmp) == 0){
	  message("There is no interaction in the decision ensemble, only single variable effects.")
	} else {
	  suppressMessages(
	    tmp <- getDecisionsMetrics(tmp, data = data, target = target, classPos = classPos
	                               , importances = FALSE
	                               , in_parallel = in_parallel, n_cores = n_cores, cluster = cluster)
	    )
	  rulesRm <- tmp[rulesRm, on = 'condition']
	}

	# add the metrics for the "empty decisions" (original rule = a single variable only)
	tmp <- measureAll(data = data, target = target, classPos = classPos)
	rulesRm <- rulesRm[condition == '', `:=`(len = 0, support = 1, err = tmp['err'], pred = tmp['pred'])]


	# get the absolute support size for all
	nall <- nrow(data)
	rules <- rules[,'support_abs' := nall*support]
	rulesRm <- rulesRm[,'support_abs' := nall*support]

	return(list('original' = rules
				, 'rm' = rulesRm, 'directions'=directions))

}





####################################################################



### Get the complements for individual rules
complementSingleRule <- function(rule){
  rule <- unlist(rule)
  ruleExec <- unlist(strsplit(rule['condition'], split = ' & '))
  vars <- str_extract(ruleExec, pattern = '(?<=X\\[,)[:digit:]+')
  # order per variable
  tmp <- order(vars)
  ruleExec <- ruleExec[tmp]
  vars <- vars[tmp]
  # group conditions on a same variable
  in_dup <- which(duplicated(vars))
  if (length(in_dup)>0){
    for (i in in_dup){ruleExec[i-1] <- paste(ruleExec[i-1], ruleExec[i], sep = " & ")}
    ruleExec <- ruleExec[-in_dup]
    vars <- vars[-in_dup]
  }

  ruleID <- rule['ruleID']
  signs <- c("<=" = 'a', '>' = "b", 'a' = '>', "b" = "<=") #need that trick to be sure replace them all when there are 2!
  newRules <- list()
  s <- list()

  for (i in 1:length(ruleExec)){
    if(str_detect(ruleExec[i], pattern = "&")){
      comp <- str_replace_all(ruleExec[i], signs)
      comp <- paste0("(",str_replace_all(comp, pattern = '&', replacement = '|'),")")
      sV <- paste0("mean(unlist(data[which(", ruleExec[i],"),",vars[i],"])) > mean(unlist(data[which(", comp,"),",vars[i],"]))")
      sV <- str_replace_all(sV, pattern = '(?<=\\(| )X(?=\\[)', replacement = 'data')
    } else {
      if (str_detect(ruleExec[i], pattern = ">")){
        sV <- 'TRUE'
      } else { sV <- 'FALSE'}
    }

    rm <- paste(ruleExec[-i], collapse = " & ")

    newRules[[paste0(ruleID,'varRm', vars[i])]] <- rm
    s[[paste0(ruleID,'var', vars[i])]] <- sV
  }

  return(list('nR' = newRules, 's' = s))
}
