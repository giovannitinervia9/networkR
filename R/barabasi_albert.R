#' Simulate a Barabási–Albert Network
#'
#' Generates a scale-free network using the Barabási–Albert (BA) preferential attachment model.
#'
#' @param n_nodes_final Integer. The final number of nodes in the network. Must be greater than `n_nodes_initial`.
#' @param new_nodes_connections Integer. Number of connections each new node makes to existing nodes.
#' @param n_nodes_initial Integer. Initial number of nodes. Default is 20.
#' @param n_edges_initial Integer. Initial number of edges. Default is 50.
#' @param expected Logical. If `FALSE`, each new node makes exactly `new_nodes_connections` connections;
#'   if `TRUE`, the number of connections per new node is sampled from a Poisson distribution with mean `new_nodes_connections`.
#'   Default is `FALSE`.
#' @param progress Logical. Display a progress bar during network generation. Default is `FALSE`.
#' @param seed Optional integer. A random seed for reproducibility.
#'
#' @return An object of class `network` representing the simulated BA network.
#' @examples
#' # Simulate a BA network with 100 nodes, each new node with 3 connections
#' ba_network <- simulate_ba_net(100, 3)
#'
#' @importFrom stats rpois
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
simulate_ba_net <- function(n_nodes_final, new_nodes_connections,
                            n_nodes_initial = 20, n_edges_initial = 50,
                            expected = FALSE, progress = FALSE, seed = NULL) {
  # Input validation
  if (n_nodes_final <= n_nodes_initial) {
    stop("n_nodes_final must be greater than n_nodes_initial")
  }
  if (new_nodes_connections > n_nodes_initial) {
    stop("new_nodes_connections cannot exceed n_nodes_initial")
  }

  # Set random seed if provided
  if (!is.null(seed)) {
    old_seed <- .Random.seed
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
    set.seed(seed)
  }

  # Initialize with Erdős-Rényi network
  net <- simulate_er_net(n_nodes_initial, n_edges_initial, TRUE, seed = seed)
  dn <- network_degree(net)
  degree_net <- dn$nodes_degree$degree
  nodes <- dn$nodes_degree$nodes

  # Grow network
  new_nodes <- (n_nodes_initial + 1):n_nodes_final
  total_new <- length(new_nodes)
  adj_list <- network_to_adj_list(net)

  if (progress) {
    pb <- txtProgressBar(min = 0, max = total_new, style = 3)
  }

  # Determine the number of connections for each new node
  if (expected == FALSE) {
    new_nodes_connections <- rep(new_nodes_connections, total_new)
  } else {
    new_nodes_connections <- pmax(1, rpois(total_new, new_nodes_connections))
  }

  # Add new nodes one by one
  for (i in seq_along(new_nodes)) {

    # Calculate probabilities based on current degrees
    p <- degree_net / sum(degree_net)

    # Sample nodes to connect the new node to
    new_edges <- sample(nodes, new_nodes_connections[i], replace = FALSE, prob = p)
    adj_list[[new_nodes[i]]] <- new_edges

    # Update degree_net and nodes to include the new node
    degree_net <- c(degree_net, new_nodes_connections[i])
    degree_net[match(new_edges, nodes)] <- degree_net[match(new_edges, nodes)] + 1
    nodes <- c(nodes, new_nodes[i])

    # Update progress bar
    if (progress) { setTxtProgressBar(pb, i) }
  }

  if (progress) close(pb)
  names(adj_list) <- 1:n_nodes_final

  # Convert adjacency list back to network format
  net <- adj_list_to_network(adj_list)
  return(net)
}
