#' Plot the importance and influence of features.
#'
#' Returns a ggplot object with variables importance (across all categorical levels for factor variables) and variable per-level influence. 
#' It uses the ggpubr package to combine plots.
#' 
#' @param decision_ensemble stable decision ensemble (see stabilitySelection).
#' @param levels_order optional, order for variables levels on the influence plot 
#' @param return_all TRUE, returns the table of feature importance and influences and each plot separated (default = FALSE).
#' @return 2 ggplots arranged in a row with ggpubr; if return_all = TRUE, returns plots separately in a list , as well as the tables used to create plots.
#' @export
plotFeatures <- function(decision_ensemble, levels_order = NULL
    , colour_low = "#E69F00", colour_mid = "grey87", colour_high = "#0072B2"
    , return_all = FALSE){


    # get the importance across all levels for each feature
    agg_imp <- decision_ensemble$nodes
    agg_imp$Feature <- str_replace(agg_imp$var, pattern = '\\_{2}.*', replacement = '')
    agg_imp <- agg_imp %>% group_by(Feature) %>% summarize(importance = sum(importance))
    # order features based on their importance
    agg_imp$Feature <- factor(agg_imp$Feature, levels = agg_imp$Feature[order(agg_imp$importance)])
    
    # make the feature importance plot
    paggimp <- ggplot(agg_imp, aes(x = Feature, y = importance)) + 
        geom_col(fill = "lightgray", width = 0.1) + geom_point(size = 3) + 
        coord_flip() + theme_classic() + theme(axis.text.y = element_text(size = rel(1.2)), 
        axis.text.x = element_text(size = rel(1.2)), legend.text = element_text(size = rel(1.2)), 
        panel.grid.major.y = element_line(colour = "grey", size = 0.2)) + 
        labs(x = "", y = "Importance", title = "Feature importance")
    

    # arrange the feature influences
    agg_inf <- decision_ensemble$nodes %>% select(var, influence)
    agg_inf$level <- str_extract(agg_inf$var, pattern = "(?<=\\_{2}).*")
    agg_inf$Feature_short <- str_replace(agg_inf$var, pattern = "\\_{2}.*", replacement = '') 
    agg_inf <- left_join(agg_inf, agg_imp, by = c(Feature_short = "Feature"))
    l <- unique(agg_inf$level)
    l <- l[!is.na(l)]

    if (length(l) > 0){
        ix <- which(is.na(agg_inf$level))
        tmp <- agg_inf[rep(ix, length(l)), ]
        tmp$level <- unlist(lapply(l, function(x) rep(x, length(ix))))
        agg_inf <- agg_inf[complete.cases(agg_inf), ] %>% rbind(tmp)
        agg_inf$Feature_short <- factor(agg_inf$Feature_short, levels = unique(agg_inf$Feature_short[order(agg_inf$importance)]))
        if (!is.null(levels_order)) {
            agg_inf$level <- factor(agg_inf$level, levels = levels_order)
        }
    } else {agg_inf$level <- ''}

    level_inf <- ggplot(agg_inf, aes(x = level, y = Feature_short, 
        fill = influence)) + geom_tile(color = "white") + theme_classic() + 
        theme(axis.text.y = element_blank(), axis.text.x = element_text(size = rel(1.2)), 
            legend.text = element_text(size = rel(1.2)), legend.title = element_text(size = rel(1.2), 
                vjust = 0.85)) + labs(x = "Level", y = "", title = "Influence per level") + 
        scale_fill_gradient2(low = colour_low, mid = colour_mid, 
            high = colour_high, midpoint = 0, name = "Influence on\nphenotype")
    
    paggs <- ggpubr::ggarrange(paggimp + theme(legend.position = "none"), 
        level_inf, nrow = 1, ncol = 2, widths = c(1, 0.8))

    if (return_all == TRUE) {
        return(list('importance' = agg_imp, 'importance_p' = paggimp
                    , 'influences' = agg_inf, 'influence_p' = level_inf))
    } else {return(paggs)}
    
}