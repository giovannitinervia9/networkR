#' Convert Graph List to Network Object
#'
#' This function takes a graph represented as a list of undirected edges and
#' converts it into a `network` object with properties such as nodes, edges,
#' and mean degree. The graph should be a list where each element is an integer
#' vector of length 2 representing an edge between two nodes.
#'
#' @param graph A list of integer pairs, where each pair represents an undirected
#' edge in the network. Each pair should contain exactly two positive integer
#' values, with each value representing a node.
#'
#' @return An object of class `network`, a list with the following components:
#'   \item{graph}{A list of unique, sorted integer pairs representing edges.}
#'   \item{nodes}{A sorted vector of unique node indices in the graph.}
#'   \item{n_nodes}{The total number of unique nodes in the graph.}
#'   \item{n_edges}{The total number of unique edges in the graph.}
#'   \item{mean_degree}{The mean degree of nodes in the network, calculated as
#'   `2 * n_edges / n_nodes`.}
#'
#' @examples
#' # Example of creating a graph and converting it to a network object
#' graph <- list(c(1, 2), c(2, 3), c(3, 1), c(1, 2)) # Duplicate edges included
#' network <- graph_to_network(graph)
#' print(network)
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
      mean_degree = 2*n_edges/n_nodes
    ),
    class = "network"
  )



  network
}




#' Print Summary for Network Object
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


