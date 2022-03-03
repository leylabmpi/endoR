#' Plot an interaction variable network.
#'
#' Returns a plot of nodes and edges. Plots are created with the ggraph and igraph packages.
#'
#' @param decision_ensemble stable decision ensemble: list with edges, nodes, etc.
#' @param path_length maximal number of edges between 2 nodes, default = Inf.
#' @param layout network layout, default is 'stress' (see ggraph package)
#' @param hide_isolated_nodes logical, default = TRUE (= nodes without any edge are not shown).
#' @param colour_x_y colour of x = edge or node, and y = low, mid or high (e.g., colour_edge_mid), to use for the colour gradients.
#' @param text_size size of node labels.
#' @param seed the seed to use for generating the network.
#' @return a ggraph object
#' @export

plotNetwork <- function(decision_ensemble, path_length = Inf
	, layout = "stress"
	, colour_edge_low = "#E69F00", colour_edge_mid = "grey87", colour_edge_high = "#0072B2"
	, colour_node_low = "#E69F00", colour_node_mid = "grey87", colour_node_high = "#0072B2"
	, text_size = 4, hide_isolated_nodes = TRUE, seed = 0){

  require(ggraph)
  require(igraph)

	network <- graph_from_data_frame(d = decision_ensemble$edges, vertices = decision_ensemble$nodes
		, directed = FALSE)

  if (path_length != Inf){

    suppressWarnings(
      sh_paths <- lapply(1:length(V(network)), shortest_paths, graph = network, to = V(network)
                         , output = 'both')
    )

    # get path length and the maximal one
    tmp <- lapply(sh_paths, function(x){sapply(x$epath, length)})
    max_length <- max(unlist(tmp))

    # loop to remove edges until we reach the maximal path length
    while(max_length > path_length){
      # subset paths to the longest ones
      i_max <- lapply(tmp, function(x){which(x == max_length)})
      e_max <- lapply(1:length(i_max), function(i, v, i_max){ v[[i]]$epath[ i_max[[i]] ] }
                      , v = sh_paths, i_max=i_max)
      e_max <- e_max[sapply(e_max, length) > 0]

      # get the importances of each edge in paths
      imp_min <- list()
      for (i in 1:length(e_max)){
        tmp <- lapply(e_max[[i]], function(x) x$importance)
        imp_min[[i]] <- lapply(tmp, function(x) which.min(x))
      }

      # get the edges with lowest importance
      to_rm <- c()
      for (i in 1:length(imp_min)){
        for (j in 1:length(e_max[[i]])){
          to_rm <- c(to_rm, as_ids(e_max[[i]][[j]][imp_min[[i]][[j]] ]) )
        }
      }
      to_rm <- unique(to_rm)

      # remove them from the edge
      network <- delete_edges(network, to_rm)

      # update the max length
      suppressWarnings(
        sh_paths <- lapply(1:length(V(network)), shortest_paths, graph = network, to = V(network)
                           , output = 'both')
      )
      tmp <- lapply(sh_paths, function(x){sapply(x$epath, length)})
      max_length <- max(unlist(tmp))
    }

  }


	### Now plot!

	# remove isolated nodes
	if (hide_isolated_nodes == TRUE){
		network <- delete.vertices(network, degree(network)==0)
	}

	if (length(unique(decision_ensemble$edges$d_assoc)) == 2) {
        linetype <- c("dashed", "solid")
    }
    else if (length(unique(decision_ensemble$edges$d_assoc)) ==
        1) {
        linetype <- "solid"
    }
    lim <- c(min(min(decision_ensemble$nodes$influence), min(decision_ensemble$edges$influence)),
        max(max(decision_ensemble$nodes$influence), max(decision_ensemble$edges$influence)))
    set.seed(seed)
    p <- ggraph(network, layout = layout) + geom_edge_fan(aes(color = influence,
        linetype = d_assoc, alpha = importance, width = importance)) +
        geom_node_point(aes(size = importance, fill = influence),
            color = "black", shape = 21) + geom_node_text(aes(label = name),
        repel = TRUE, size = text_size) + theme_graph() + guides(edge_alpha = "none",
        edge_linetype = "none", edge_colour = "none") + scale_edge_linetype_manual(values = linetype) +
        scale_edge_colour_gradient2(low = colour_edge_low, mid = colour_edge_mid,
            high = colour_edge_high, midpoint = 0, limits = lim) +
        scale_fill_gradient2(low = colour_node_low, mid = colour_node_mid,
            high = colour_node_high, midpoint = 0, limits = lim) +
        scale_size_continuous(limits = c(0, max(max(decision_ensemble$nodes$importance),
            max(decision_ensemble$edges$importance))), guide = guide_legend(override.aes = list(fill = "white"))) +
        scale_edge_width_continuous(limits = c(0, max(max(decision_ensemble$nodes$importance),
            max(decision_ensemble$edges$importance))))
    return(p)

}
