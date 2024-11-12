#' Convert Network to Adjacency List
#'
#' Converts a network object into an adjacency list, where each node is
#' associated with a list of its adjacent (connected) nodes.
#'
#' @param network An object of class `network` containing the network's nodes and edges.
#'
#' @return A list of class `adj_list`, where each element corresponds to a node
#'   in the network. Each element is a sorted vector of nodes that are adjacent
#'   (connected) to the given node.
#'
#' @details
#' This function iterates through each edge in the `network` object to populate
#' an adjacency list. The adjacency list is stored as a list of vectors, with
#' each vector representing the neighbors of a given node. For consistent ordering,
#' each adjacency list is sorted.
#'
#' @examples
#' # Define a simple network
#' graph <- list(c(1, 2), c(2, 3), c(3, 1), c(1, 4))
#' network <- graph_to_network(graph)
#'
#' # Convert to adjacency list
#' adj_list <- network_to_adj_list(network)
#' print(adj_list)
#'
#' @export
network_to_adj_list <- function(network) {
  if (!inherits(network, "network")) {
    stop("network must be an object of class network")
  }

  adj_list <- vector("list", network$n_nodes)
  names(adj_list) <- as.character(sort(network$nodes))

  for (edge in network$graph) {
    i <- as.character(edge[1])
    j <- as.character(edge[2])
    adj_list[[i]] <- c(adj_list[[i]], edge[2])
    adj_list[[j]] <- c(adj_list[[j]], edge[1])
  }

  adj_list <- lapply(adj_list, sort)
  class(adj_list) <- "adj_list"
  adj_list
}


#' Convert Adjacency List to Network
#'
#' Converts an adjacency list object back into a network object with a list of edges.
#'
#' @param adj_list A list of class `adj_list`, where each element is a vector
#'   of nodes that are adjacent (connected) to the key node.
#'
#' @return An object of class `network`, created by reconstructing edges from
#'   the adjacency list. The resulting network object contains the nodes, edges,
#'   and summary statistics of the network.
#'
#' @details
#' This function reconstructs the network's edges from an adjacency list. Each
#' pair of connected nodes is treated as an undirected edge, so duplicate edges
#' are removed to ensure each edge appears only once in the network.
#'
#' @examples
#' # Create a sample adjacency list
#' adj_list <- list(
#'   `1` = c(2, 3, 4),
#'   `2` = c(1, 3),
#'   `3` = c(1, 2),
#'   `4` = c(1)
#' )
#' class(adj_list) <- "adj_list"
#'
#' # Convert to network object
#' network <- adj_list_to_network(adj_list)
#' print(network)
#'
#' @export
adj_list_to_network <- function(adj_list) {
  if (!inherits(adj_list, "adj_list")) {
    stop("adj_list must be an object of class adj_list")
  }

  graph <- vector("list", 0)
  nodes <- as.integer(names(adj_list))

  for (i in seq_along(adj_list)) {
    node_i <- nodes[i]
    neighbors <- adj_list[[i]]
    new_edges <- lapply(neighbors, function(j) c(node_i, j))
    graph <- c(graph, new_edges)
  }

  graph <- unique(lapply(graph, sort))

  graph_to_network(graph)
}
