#' Simulate an Erdős-Rényi Network
#'
#' Simulates an Erdős-Rényi (ER) random network with a specified number of nodes and edges.
#'
#' @param n_nodes Integer. The number of nodes in the network. Must be at least 2.
#' @param n_edges Integer. The (expected) number of edges in the network. For `expected = FALSE`,
#'   this value must not exceed the maximum number of possible edges (`n_nodes * (n_nodes - 1) / 2`).
#' @param expected Logical. If `FALSE`, the function creates a network with exactly `n_edges`.
#'   If `TRUE`, it uses the probability method, where each edge is included with probability `p = n_edges / max_edges`.
#'   Default is `FALSE`.
#' @param seed Optional integer. A random seed for reproducibility.
#'
#' @return An object of class `network` representing the simulated Erdős-Rényi network.
#'
#' @details
#' This function generates an ER network in one of two ways:
#' * If `expected = FALSE`, it selects exactly `n_edges` randomly chosen edges.
#' * If `expected = TRUE`, it assigns each possible edge a probability of inclusion based on the
#'   desired `n_edges`, creating a network with an *expected* number of edges close to `n_edges`.
#'
#' @note
#' When `expected = TRUE`, the actual number of edges may vary around `n_edges`.
#'
#' @examples
#' # Simulate an ER network with 10 nodes and 15 edges
#' er_network <- simulate_er_net(10, 15)
#'
#' # Simulate an ER network with 10 nodes, expected 15 edges
#' er_network_expected <- simulate_er_net(10, 15, expected = TRUE)
#'
#' @importFrom stats runif
#' @importFrom utils combn
#' @export
simulate_er_net <- function(n_nodes, n_edges, expected = FALSE, seed = NULL) {
  if (n_nodes < 2) stop("n_nodes must be at least 2")
  max_edges <- choose(n_nodes, 2)
  if (!expected && n_edges > max_edges) {
    stop("n_edges cannot exceed ", max_edges, " for ", n_nodes, " nodes")
  }

  if (!is.null(seed)) {
    old_seed <- .Random.seed
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
    set.seed(seed)
  }

  all_possible_links <- combn(1:n_nodes, 2, simplify = FALSE)

  if (!expected) {
    sample_ind <- sample(length(all_possible_links), n_edges, FALSE)
    graph <- all_possible_links[sample_ind]
  } else {
    p <- n_edges / max_edges
    sample_ind <- which(runif(max_edges) < p)
    graph <- all_possible_links[sample_ind]
  }

  graph_to_network(graph)
}
