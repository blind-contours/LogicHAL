#' Plot Logic Tree
#'
#' This function takes a decision tree structure with F1 scores and visualizes it using ggraph.
#'
#' @param tree A list representing the decision tree structure with splits, F1 scores, and path F1 scores.
#'
#' @examples
#' # Assuming 'tree' is a list representing the decision tree structure
#' plot_logic_tree(tree)
#' @import igraph
#' @import ggraph
#' @import ggplot2
#' @export
plot_logic_tree <- function(tree) {
  # Create an empty igraph object
  g <- make_empty_graph(directed = TRUE)

  # Recursive function to add nodes and edges with edge coloring
  add_node <- function(graph, node, parent = NULL, edge_type = NULL) {
    if (is.null(node)) {
      return(graph)
    }

    # Extract split and F1 information
    vertex_label <- paste(node$split, "\nF1:", node$f1)

    # Add vertex with label
    graph <- add_vertices(graph, 1, name = vertex_label)
    current_vertex <- V(graph)$name[length(V(graph)$name)]

    # Add edge to parent if necessary
    if (!is.null(parent)) {
      graph <- add_edges(graph, c(parent, current_vertex), color = ifelse(edge_type == "left", "black", "red"))
    }

    # Recursive calls for left and right subtrees with edge type information
    if (!is.null(node$left)) {
      graph <- add_node(graph, node$left, current_vertex, edge_type = "left")
    }
    if (!is.null(node$right)) {
      graph <- add_node(graph, node$right, current_vertex, edge_type = "right")
    }

    return(graph)
  }

  # Build the tree graph
  g <- add_node(g, tree)

  # Plot the graph using ggraph with customized legend
  ggraph(g, layout = "tree") +
    geom_edge_link(aes(edge_colour = color)) +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), nudge_y = 0.2, repel = TRUE) +
    scale_edge_colour_manual(
      values = c("black", "red"),
      labels = c("TRUE", "FALSE"),
      name = "Edge Type"
    ) +
    theme_void() +
    labs(title = "Logic Tree")
}
