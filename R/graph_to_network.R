#' Convert List of Edges to Network Object
#'
#' Converts a graph represented as a list of undirected edges into a `network` object with structured
#' information on nodes, edges, mean degree, and optional node attributes. This function expects each edge to be
#' an integer vector of length 2, indicating an undirected connection between two nodes.
#'
#' @param graph A list of integer pairs, where each pair represents an undirected edge in the network.
#' Each element in the list should be a vector of exactly two positive integers, with each integer indicating a node index.
#'
#' @return A `network` class object, which is a list containing the following components:
#'   \item{graph}{A list of unique, sorted integer pairs representing the undirected edges in the network.}
#'   \item{nodes}{A sorted vector of unique node indices present in the network.}
#'   \item{n_nodes}{The total number of unique nodes in the network.}
#'   \item{n_edges}{The total number of unique edges in the network.}
#'   \item{mean_degree}{The mean degree of nodes in the network, calculated as `2 * n_edges / n_nodes`.}
#'   \item{attributes}{An empty `data.frame` for storing node attributes, with row names as node indices.}
#'
#' @examples
#' # Define a graph with nodes and edges
#' graph <- list(c(1, 2), c(2, 3), c(3, 1), c(1, 2)) # Includes duplicate edges
#' network <- graph_to_network(graph)
#' network
#'
#' @export
graph_to_network <- function(graph) {
  # Input validation
  if (!is.list(graph) || !all(sapply(graph, length) == 2)) {
    stop("graph must be a list of integer pairs representing edges")
  }

  # Additional validation for edge values
  edge_values <- unlist(graph)

  if (!all(is.numeric(edge_values))) {
    stop("all edges must contain numeric values")
  }
  if (!all(edge_values == floor(edge_values))) {
    stop("all edges must contain integer values")
  }
  if (any(edge_values < 1)) {
    stop("all node indices must be positive integers")
  }


  nodes <- sort(unique(edge_values))


  # Ensure all edges are sorted for consistency
  graph <- lapply(graph, sort)
  graph <- unique(graph)  # Remove any duplicate edges

  net_mat <- do.call(rbind, graph)
  net_mat <- net_mat[order(net_mat[, 1], net_mat[, 2]), ]
  graph <- split(net_mat, row(net_mat), drop = T)

  n_nodes <- length(nodes)
  n_edges <- length(graph)


  # Create and validate network object
  network <- structure(
    list(
      graph = graph,
      nodes = nodes,
      n_nodes = n_nodes,
      n_edges = n_edges,
      mean_degree = 2*n_edges/n_nodes,
      attributes = data.frame(row.names = nodes)
    ),
    class = "network"
  )



  network
}




#' Print method for Network Object
#'
#' This is a print method for objects of class `network`, displaying basic
#' summary information about the network, including the number of nodes, edges,
#' and mean degree.
#'
#' @param x An object of class `network`.
#' @param digits Integer specifying the number of significant digits to use
#' for displaying the mean degree. Defaults to the maximum of 3 or the current
#' option set by `getOption("digits") - 3`.
#' @param ... Additional arguments passed to other methods (not used here).
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' # Create a network object and print summary
#' graph <- list(c(1, 2), c(2, 3), c(3, 1))
#' network <- graph_to_network(graph)
#' print(network)
#'
#' @export
print.network <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  # Print number of nodes
  cat("Number of nodes:", x$n_nodes, "\n")

  # Print number of edges
  cat("Number of edges:", x$n_edges, "\n")

  # Print mean degree, rounded to specified digits
  cat("Mean degree:", round(x$mean_degree, digits), "\n")

  # Return the object invisibly
  invisible(x)
}


#' Attach Attributes to Network Nodes
#'
#' Adds a `data.frame` of attributes to the nodes in a `network` object. Each row in `attributes` corresponds to a node,
#' with row names matching the node indices in the network. The function verifies the correct number of attributes
#' to ensure each node has one associated row of attributes.
#'
#' @param network A `network` object created by `graph_to_network`.
#' @param attributes A `data.frame` where each row represents attributes for a node in `network`.
#' The number of rows in `attributes` must equal the number of unique nodes in `network`.
#'
#' @return A modified `network` object with the `attributes` field populated by the input `data.frame`.
#'
#' @examples
#' graph <- list(c(1, 2), c(2, 3), c(3, 1))
#' network <- graph_to_network(graph)
#' attributes <- data.frame(type = c("A", "B", "C"), row.names = c(1, 2, 3))
#' network <- attach_attributes(network, attributes)
#' print(network$attributes)
#'
#' @export
attach_attributes <- function(network, attributes){
  if(!is.data.frame(attributes)){stop("attributes must be a data.frame")}
  if(nrow(attributes) != network$n_nodes){stop("nrow(attributes) must be equal to the number of the nodes")}
  network$attributes <- attributes
  network
}

