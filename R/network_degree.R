#' Calculate Node Degrees and Degree Distribution for a Network
#'
#' This function computes the degree for each node in a network and generates
#' a degree distribution summary, including absolute and relative frequencies,
#' cumulative frequencies, and survival probabilities.
#'
#' @param network An object of class `network`.
#'
#' @return An object of class `network_degree`, containing:
#'   \item{nodes_degree}{A data frame with each node and its degree.}
#'   \item{degree_distribution}{A data frame with the degree distribution,
#'   including absolute frequency, relative frequency, cumulative frequency,
#'   and survival function for each degree value.}
#'
#' @examples
#' # Create a network object
#' graph <- list(c(1, 2), c(2, 3), c(3, 1), c(1, 4))
#' network <- graph_to_network(graph)
#'
#' # Calculate node degrees and degree distribution
#' degree_info <- network_degree(network)
#' print(degree_info$nodes_degree)
#' print(degree_info$degree_distribution)
#'
#' @importFrom stats sd
#'
#' @export
network_degree <- function(network) {
  if (!inherits(network, "network")) {
    stop("network must be an object of class network")
  }

  # Calculate node degrees
  flat_nodes <- unlist(network$graph)
  degree_nodes <- table(factor(flat_nodes, levels = network$nodes))

  # Create nodes_degree dataframe
  nodes_degree <- data.frame(
    nodes = as.integer(names(degree_nodes)),
    degree = as.vector(degree_nodes)
  )

  # Calculate degree distribution statistics
  degree_table <- table(nodes_degree$degree)
  degree <- as.integer(names(degree_table))
  n_total <- sum(degree_table)

  # Create degree distribution dataframe
  degree_distr <- data.frame(
    degree = degree,
    abs_freq = as.vector(degree_table),
    rel_freq = as.vector(degree_table) / n_total,
    cum_freq = cumsum(as.vector(degree_table)) / n_total,
    surv = rev(cumsum(rev(as.vector(degree_table)))) / n_total
  )

  # Create and return network_degree object
  structure(
    list(
      nodes_degree = nodes_degree,
      degree_distribution = degree_distr
    ),
    class = "network_degree"
  )
}




#' Print Summary of Node Degree Distribution
#'
#' This is a print method for objects of class `network_degree`, displaying
#' summary statistics of node degrees, including the minimum, first quartile,
#' median, mean, third quartile, maximum, and standard deviation.
#'
#' @param x An object of class `network_degree`.
#' @param digits Integer specifying the number of significant digits to use
#' when printing summary statistics. Defaults to the maximum of 3 or
#' `getOption("digits") - 3`.
#' @param ... Additional arguments passed to other methods (not used here).
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' # Create a network and calculate degree information
#' graph <- unique(matrix(sample(1:100, 1000, TRUE), ncol = 2))
#' graph <- split(graph, row(graph))
#' network <- graph_to_network(graph)
#' degree_info <- network_degree(network)
#'
#' # Print summary of the degree distribution
#' print(degree_info)
#'
#' # Plot
#' plot(degree_info, 1)
#' plot(degree_info, 2)
#' plot(degree_info, 3)
#' @export
print.network_degree <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  # Extract node degrees
  deg <- x$nodes_degree$degree

  # Calculate summary statistics for degrees
  summ <- c(as.vector(summary(deg)), sd(deg))
  names(summ) <- c("Min", "1Q", "Median", "Mean", "3Q", "Max", "SD")

  cat("Degree distribution summary:\n")

  # Print summary with specified number of digits
  print.default(summ, digits = digits)

  # Return the object invisibly
  invisible(x)
}



#' Plot Degree Distribution for `network_degree` Object
#'
#' This is a plot method for objects of class `network_degree`, providing
#' visualization of the degree distribution as density, empirical cumulative
#' distribution function (ECDF), and log survival function (log-log plot).
#'
#' @param x An object of class `network_degree`.
#' @param which An integer vector specifying which plots to display. Choices
#' are:
#' - `1`: Density plot of node degree distribution.
#' - `2`: ECDF of the node degrees.
#' - `3`: Log-log plot of the survival function.
#' Defaults to showing all three plots (`1:3`).
#' @param ... Additional arguments (not used here).
#'
#' @details
#' The function generates three plots:
#' \itemize{
#'   \item **Density Plot** (Plot 1): Shows the relative frequency of each degree.
#'   \item **ECDF Plot** (Plot 2): Shows the cumulative distribution of degrees.
#'   \item **Log Survival Plot** (Plot 3): A log-log plot of the survival function
#'   with a linear fit, useful for assessing the tail behavior of the degree distribution.
#' }
#'
#' @return Invisibly returns a list of `ggplot` objects for the selected plots.
#'
#' @examples
#' # Create a network and calculate degree information
#' graph <- list(c(1, 2), c(2, 3), c(3, 1), c(1, 4))
#' network <- graph_to_network(graph)
#' degree_info <- network_degree(network)
#'
#' # Plot all available degree distribution graphs
#' plot(degree_info)
#'
#' # Plot only the density and ECDF plots
#' plot(degree_info, which = 1:2)
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_line geom_smooth
#' @export
#' @importFrom rlang .data
plot.network_degree <- function(x, which = 1:3, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is required for plotting")
  }

  degree <- x$degree_distribution

  # Generate list of ggplot objects for each type of plot
  plots <- list(
    ggplot2::ggplot(degree, ggplot2::aes(x = .data$degree, y = .data$rel_freq)) +
      ggplot2::geom_col() +
      ggplot2::ylab("density") +
      ggplot2::xlab("degree"),

    ggplot2::ggplot(degree, ggplot2::aes(x = .data$degree, y = .data$cum_freq)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_line(linewidth = 0.2) +
      ggplot2::ylab("ecdf") +
      ggplot2::xlab("degree"),

    ggplot2::ggplot(degree, ggplot2::aes(x = log(.data$degree), y = log(.data$surv))) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_line(linewidth = 0.2) +
      ggplot2::ylab("log(survival function)") +
      ggplot2::xlab("log(degree)") +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.3, color = "red")
  )

  # Select and print the requested plots
  res <- plots[which]
  suppressMessages(invisible(lapply(res, print)))
  invisible(res)
}

