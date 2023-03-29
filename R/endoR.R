#' endoR
#'
#' endoR extracts and visualizes how predictive variables contribute to tree ensemble model accuracy.
#'
#' @docType package
#'
#' @author Albane Ruaud \email{albane.ruaud@tuebingen.mpg.de}
#'
#' @name endoR
#' @import data.table
#' @rawNamespace import(dplyr, except = c(union, as_data_frame, groups, combine, slice, filter, lag, last, first, between))
#' @import inTrees
#' @import stringr
#' @rawNamespace import(ggplot2, except = margin)
#' @import ggraph
#' @rawNamespace import(igraph, except = c(is_named, decompose, spectrum))
#' @import stats
#' @import parallel
#' @import clustermq
#' @rawNamespace import(rlang, except = ':=')
#' @import utils
#' @rawNamespace import(randomForest, except = importance)
#' @import tidyverse
#' @import caret
#' @import ranger
#' @import xgboost

utils::globalVariables(c('Feature', 'Feature_short', 'association_sign'
                         , 'change', 'changed', 'classPos', 'coarseFeature'
                         , 'condition', 'd', 'd.x', 'd.y', 'd_assoc', 'decay'
                         , 'dummy_var', 'elongateTaxa', 'err', 'err.rm', 'f'
                         , 'filt_imp', 'full', 'fullID', 'g', 'gain', 'imp'
                         , 'impThr', 'imp_sd', 'importance.x', 'importance.y'
                         , 'inN', 'in_parallel', 'ix', 'k', 'len', 'lev'
                         , 'level', 'maxDecay', 'n_cores', 'n_sd', 'name'
                         , 'newFeature', 'newWeight', 'nremoved', 'nrules', 'o'
                         , 'oldN', 'p', 'pred', 'pred.rm', 'predRm.x'
                         , 'predRm.xy', 'predRm.y', 'prune', 'ruleID', 's'
                         , 's.rm', 'sRm.x', 'sRm.y', 'sum_impn', 'support'
                         , 'target', 'to_up', 'to_update', 'to_update_ctg'
                         , 'typeDecay', 'weight', 'wimp', 'x', 'y'))


#' This is data to be included in my package
#'
#' @author Paul Hendricks \email{paul.hendricks.2013@owu.edu}
#' @references \url{https://cran.r-project.org/package=titanic}
"titanic"

