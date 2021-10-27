#' Plot the importance and influence of features.
#'
#' Returns a ggplot object with variables importance (across all categorical levels for factor variables) and variable per-level influence. 
#' It uses the ggpubr package to combine plots.
#' 
#' @param decision_ensemble stable decision ensemble (see stabilitySelection).
#' @param levels_order optional, order for variables levels on the influence plot 
#' @return 2 ggplots arranged in a row with ggpubr.
#' @export
plotFeatures <- function(decision_ensemble, levels_order = NULL
                        , colour_low = "#E69F00", colour_mid = "grey87", colour_high = "#0072B2"){
	
### Make the feature importance plot
# Get importances across levels
tmp <- decision_ensemble$rules_summary %>% subset(inN >= decision_ensemble$parameters['pi_thr'], select = condition) %>% unlist
agg_imp <- featureImportance(decision_ensemble$nodes_agg %>% subset(condition %in% tmp) ) 
agg_imp$Feature <- factor(agg_imp$Feature, levels = agg_imp$Feature[order(agg_imp$importance)])
# Plot
paggimp <- ggplot(agg_imp, aes(x = Feature, y = importance)) + 
geom_col(fill = 'lightgray', width = .1) + 
geom_point(size = 3) + coord_flip() + 
theme_classic() + 
theme(axis.text.y = element_text(size = rel(1.2)), axis.text.x = element_text(size = rel(1.2))
      ,legend.text = element_text(size = rel(1.2))
      ,panel.grid.major.y = element_line(colour = "grey", size = .2)) + 
labs(x = '', y = 'Importance', title = 'Aggregated importance') 


### Make the level influence
# Format nodes table to get all info per level
agg_inf <- decision_ensemble$nodes %>% select(var, influence)
agg_inf$level <- str_extract(agg_inf$var, pattern = '(?<=\\_{2}).*')
agg_inf$Feature_short <- str_extract(agg_inf$var, pattern = '.*(?=\\_{2})')%>% 
                    str_replace(pattern = '(?<=s\\_[:upper:])[:lower:]+(?=\\_)', replacement = '')
agg_inf$Feature_short <- ifelse(is.na(agg_inf$Feature_short), agg_inf$var, agg_inf$Feature_short)
agg_inf <- left_join(agg_inf, agg_imp, by = c('Feature_short' = 'Feature'))
# Spread across levels for numeric variables
l <- unique(agg_inf$level)
l <- l[!is.na(l)]
ix <- which(is.na(agg_inf$level))
tmp <- agg_inf[rep(ix, length(l)), ]
tmp$level <- unlist(lapply(l, function(x)rep(x,length(ix))))
agg_inf <- agg_inf[ complete.cases(agg_inf), ] %>% rbind(tmp)

agg_inf$Feature_short <- factor(agg_inf$Feature_short
                                , levels = unique(agg_inf$Feature_short[order(agg_inf$importance)]))
if (!is.null(levels_order)){agg_inf$level <- factor(agg_inf$level, levels = levels_order)}

# Plot
level_inf <- ggplot(agg_inf, aes(x = level, y = Feature_short, fill= influence)) + geom_tile(color = 'white')+
theme_classic()+
theme(axis.text.y = element_blank()
      ,axis.text.x = element_text(size = rel(1.2))
      ,legend.text = element_text(size = rel(1.2))
      , legend.title=element_text(size=rel(1.2), vjust = .85)
      ) + 
labs(x = 'Level', y = '', title = 'Influence per level') +

scale_fill_gradient2(low = colour_low, mid = colour_mid, high = colour_high, midpoint = 0, name = 'Influence on\nphenotype')

### Combine plots
paggs <- ggpubr::ggarrange(paggimp+theme(legend.position = 'none')
                   ,level_inf
                   , nrow = 1, ncol = 2, widths = c(1,.8))
return(paggs)

}