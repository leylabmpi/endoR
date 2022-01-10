treeVisit_endoR<- #### remove the maxdepth argument and returns only full length rules
function(tree,rowIx,count,ruleSet,rule,levelX,length,max_length){ ###
  if( tree[rowIx,"status"] == -1 | length == max_length ){
    count <- count + 1
    ruleSet[[count]] <- rule
    return(list(ruleSet = ruleSet, count=count))
  }

  xIx <- tree[rowIx,"split var"]
  xValue <- tree[rowIx,"split point"]

  if(is.null(levelX[[xIx]])){
   lValue <- paste("X[,",xIx, "]<=",xValue,sep="")
   rValue <- paste("X[,",xIx, "]>",xValue,sep="")
  }else{
   xValue<- which(as.integer(intToBits(as.integer(xValue)))>0)
   lValue <- levelX[[xIx]][xValue]
   rValue <- setdiff(levelX[[xIx]],lValue)
  }  
   xValue <- NULL
   ruleleft <- rule
   if(length(ruleleft)==0){
     ruleleft[[as.character(xIx)]] <- lValue 
   }else{
     if(as.character(xIx) %in% ls(ruleleft)) {
          if(!is.null(levelX[[xIx]])){    
            lValue <- intersect(ruleleft[[as.character(xIx)]],lValue)
            # here this may be empty -> we stop here
            if (length(lValue) == 0){
                count <- count + 1
                ruleSet[[count]] <- rule
                return(list(ruleSet = ruleSet, count=count))
            }
            ruleleft[[as.character(xIx)]] <- lValue
          } else {
            ruleleft[[as.character(xIx)]] <- paste(ruleleft[[as.character(xIx)]], "&", lValue)
          }
       }else{
       ruleleft[[as.character(xIx)]] <- lValue
     }
   }
  
   ruleright <- rule
   if(length(ruleright)==0)
   {
     ruleright[[as.character(xIx)]] <- rValue
   }else{
     if(as.character(xIx) %in% ls(ruleright)) {
         if(!is.null(levelX[[xIx]])){  
           rValue <- intersect(ruleright[[as.character(xIx)]],rValue)
           # here this may be empty -> we stop here
            if (length(rValue) == 0){
                count <- count + 1
                ruleSet[[count]] <- rule
                return(list(ruleSet = ruleSet, count=count))
            }
           ruleright[[as.character(xIx)]] <- rValue
         }else{
           ruleright[[as.character(xIx)]] <- paste(ruleright[[as.character(xIx)]], "&", rValue)
         }
     }else{
        ruleright[[as.character(xIx)]] <- rValue
     }
    }
  
   thisList <- treeVisit_endoR(tree, tree[rowIx,"left daughter"],count,ruleSet,ruleleft,levelX,length+1,max_length) ###
   ruleSet <- thisList$ruleSet; count <- thisList$count
   
   thisList <- treeVisit_endoR(tree, tree[rowIx,"right daughter"],count,ruleSet,ruleright,levelX,length+1,max_length) ###
   ruleSet <- thisList$ruleSet; count <- thisList$count
   
   return(list(ruleSet = ruleSet, count=count))
}


########################################

extractDecisions <-
function(treeList,X,ntree=100,maxdepth=Inf, in_parallel = FALSE, n_cores = detectCores() - 1, cluster = NULL){ 
  levelX <- list()
  for(iX in 1:ncol(X))
  levelX <- c(levelX,list(levels(X[,iX])))
  ntree <- min(treeList$ntree,ntree)
  if (in_parallel == TRUE){
    if (is.null(cluster) == TRUE){
      message('Initiate parallelisation ... ')
      cluster <- makeCluster(n_cores)
      clusterEvalQ(cluster, library(inTrees))
      clusterEvalQ(cluster, library(endoR))
      on.exit(stopCluster(cluster))
    }
    
    allRulesList <- parLapply(cluster, 1:ntree, extractSingleTree, treeList = treeList, levelX=levelX, maxdepth=maxdepth)
  } else { allRulesList <- lapply(1:ntree, extractSingleTree, treeList = treeList, levelX=levelX, maxdepth=maxdepth) }
  
  allRulesList <- do.call(c, allRulesList)
  allRulesList <- allRulesList[!unlist(lapply(allRulesList, is.null))]
  rulesExec <- ruleList2Exec_endoR(X,allRulesList)
return(rulesExec)
}


########################################

extractSingleTree <- function(iTree, treeList, levelX, maxdepth=Inf){
    rule <- list(); count <- 0; rowIx <- 1;
    tree <- treeList$list[[iTree]]
    if(nrow(tree)<=1) return() # skip if there is no split
    ruleSet <- vector("list", length(which(tree[,"status"]==-1)))
    res <- treeVisit_endoR(tree,rowIx = rowIx,count,ruleSet,rule,levelX,length=0, max_length=maxdepth) 
    return(res$ruleSet) 
}


########################################
# I have issues spotting where and why rules are not ordered properly, if anyone spots it, please make a sign and I'll fix it :) 
singleRuleList2Exec_endoR <-
function(ruleList,typeX){ #numeric: 1; categorical: 2
  vars <- names(ruleList) # replace ls by names 
  #vars <- vars[order(as.character(vars)) #### re-ordered since pruning doesn't depend on variables order anymore

  #ruleExec <- ""
  ruleExec <- character(length(vars))

  for(i in 1:length(vars)){
    if(typeX[as.numeric(vars[i])]==2){
      values <- paste("c(", paste( paste("'", ruleList[[vars[i]]], "'", sep = ""), collapse = ","), ")", sep = "")
      tmp <- paste("X[,",vars[i], "] %in% ", values, sep="")
    } else {
      tmp <- ruleList[[vars[i]]]
    }
    ruleExec[i] <- tmp
    #if(i==1) ruleExec <- paste(ruleExec, tmp, sep="")
    #if(i>1) ruleExec <- paste(ruleExec, " & ", tmp, sep="")
  }
  ruleExec <- paste(sort(ruleExec), collapse = ' & ')
  #re-order here, not sure why doesn't work with the var ordering.. 
  #probably because was ordering on numeric and not character like everywhere else..
  return(ruleExec) 
}


########################################

ruleList2Exec_endoR <-
function(X,allRulesList){
  typeX <- getTypeX(X)
  ruleExec <- unique(t(sapply(allRulesList,singleRuleList2Exec_endoR,typeX=typeX)))
  ruleExec <- t(ruleExec)
  colnames(ruleExec) <- "condition"
  return(ruleExec)
}


########################################



#' @export
Ranger2List_endoR <- function(rf_ranger){
# fix the indexing of variables 
  formatRanger <- function(tree){
    rownames(tree) <- 1:nrow(tree)
    tree$`status` <- ifelse(tree$`terminal`==TRUE,-1,1)
    tree$`left daughter` <- tree$`leftChild` + 1
    tree$`right daughter` <- tree$`rightChild` + 1
    tree$`split var` <- tree$`splitvarID` + 1 #here, add +1 to make it start at 1 ;)
    tree$`split point` <- tree$`splitval`
    tree$`prediction` <- tree$`prediction`
    tree <- tree[,c("left daughter","right daughter","split var","split point","status")]
    tree <- as.data.frame(tree)
    return(tree)
  }
  treeList <- NULL
  treeList$ntree <- rf_ranger$num.trees
  treeList$list <- vector("list",rf_ranger$num.trees)
  for(i in 1:rf_ranger$num.trees){
    treeList$list[[i]] <- formatRanger( treeInfo(rf_ranger, tree = i) )
  }
  return(treeList)
}
