#' Plot an interaction variable network.
#'
#' Returns a plot of nodes and edges. Plots are created with the ggraph and igraph packages.
#'
#' @param decision_ensemble stable decision ensemble: list with edges, nodes, etc.
#' @param n_edges number of edges to plot; if NULL, all edges are plotted (default).
#' @param layout network layout, default is 'stress' (see ggraph package)
#' @param hide_nodes numeric, the minimal connectivity of a node. If negative, all nodes are plotted.
#' @param colour_x_y colour of x = edge or node, and y = low, mid or high (e.g., colour_edge_mid), to use for the colour gradients. 
#' @param text_size size of node labels.
#' @param seed the seed to use for generating the network.
#' @return a ggraph object
#' @export
plotNetwork <- function(decision_ensemble, n_edges = NULL
    , layout = 'stress'
    , colour_edge_low = "firebrick2", colour_edge_mid = "grey90", colour_edge_high = "dodgerblue3"
    , colour_node_low = "firebrick2", colour_node_mid = "grey90", colour_node_high = "dodgerblue3"
    , text_size = 4
    , hide_nodes = 0

    , seed = 0){


if (!is.null(n_edges)){decision_ensemble$edges <- arrange(decision_ensemble$edges, desc(importance))[1:n_edges,]}

network <- graph_from_data_frame(d = decision_ensemble$edges, vertices = decision_ensemble$nodes, directed = FALSE)
network <- delete.vertices(network, degree(network)<=hide_nodes)

if (length(unique(decision_ensemble$edges$d_assoc)) == 2){
  linetype <- c('dashed', 'solid')
} else if (length(unique(decision_ensemble$edges$d_assoc)) == 1){
  linetype <-'solid'
}

lim <- c(min(min(decision_ensemble$nodes$influence), min(decision_ensemble$edges$influence)), max(max(decision_ensemble$nodes$influence), max(decision_ensemble$edges$influence)))

### Make the plot
set.seed(seed) # to always get the same plot layout

p <- ggraph(network, layout = layout) +
    geom_edge_fan(aes(color = influence, linetype = d_assoc, alpha = importance, width = importance )) +

    geom_node_point(aes(size = importance, fill = influence), color = 'black', shape = 21) +
    geom_node_text(aes(label = name), repel = TRUE, size = text_size) +

    theme_graph() +

    guides(edge_alpha = "none", edge_linetype = 'none', edge_colour = 'none') + 

    scale_edge_linetype_manual(values = linetype) +
    scale_edge_colour_gradient2(low = colour_edge_low, mid = colour_edge_mid, high = colour_edge_high, midpoint = 0, limits = lim) +
    
    scale_fill_gradient2(low = colour_node_low, mid = colour_node_mid, high =  colour_node_high, midpoint = 0, limits = lim) + 
    scale_size_continuous(limits = c(0, max(max(decision_ensemble$nodes$importance), max(decision_ensemble$edges$importance))), guide = guide_legend(override.aes = list(fill = 'white')))+ 
    scale_edge_width_continuous(limits = c(0, max(max(decision_ensemble$nodes$importance), max(decision_ensemble$edges$importance)))) 


return(p)


}
