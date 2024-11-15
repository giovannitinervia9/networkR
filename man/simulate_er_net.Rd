% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/erdos_reny.R
\name{simulate_er_net}
\alias{simulate_er_net}
\title{Simulate an Erdős-Rényi Network}
\usage{
simulate_er_net(n_nodes, n_edges, expected = FALSE, seed = NULL)
}
\arguments{
\item{n_nodes}{Integer. The number of nodes in the network. Must be at least 2.}

\item{n_edges}{Integer. The (expected) number of edges in the network. For \code{expected = FALSE},
this value must not exceed the maximum number of possible edges (\code{n_nodes * (n_nodes - 1) / 2}).}

\item{expected}{Logical. If \code{FALSE}, the function creates a network with exactly \code{n_edges}.
If \code{TRUE}, it uses the probability method, where each edge is included with probability \code{p = n_edges / max_edges}.
Default is \code{FALSE}.}

\item{seed}{Optional integer. A random seed for reproducibility.}
}
\value{
An object of class \code{network} representing the simulated Erdős-Rényi network.
}
\description{
Simulates an Erdős-Rényi (ER) random network with a specified number of nodes and edges.
}
\details{
This function generates an ER network in one of two ways:
\itemize{
\item If \code{expected = FALSE}, it selects exactly \code{n_edges} randomly chosen edges.
\item If \code{expected = TRUE}, it assigns each possible edge a probability of inclusion based on the
desired \code{n_edges}, creating a network with an \emph{expected} number of edges close to \code{n_edges}.
}
}
\note{
When \code{expected = TRUE}, the actual number of edges may vary around \code{n_edges}.
}
\examples{
# Simulate an ER network with 10 nodes and 15 edges
er_network <- simulate_er_net(10, 15)

# Simulate an ER network with 10 nodes, expected 15 edges
er_network_expected <- simulate_er_net(10, 15, expected = TRUE)

}
