% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_edges.R
\name{make_edges}
\alias{make_edges}
\title{Calculate vertices for network visualisations.}
\usage{
make_edges(
  connections_data,
  node_id,
  node_number,
  connection_id,
  join_col = deparse(substitute(connection_id)),
  from_to_col = NULL,
  weight_col = NULL,
  weight_col_fun = NULL,
  ...
)
}
\arguments{
\item{connections_data}{Dataframe detailing connections between nodes, as generated via \code{make_connections()}. This can have duplicate rows for node - connection object combinations, to account for how edge weighting should be handled.}

\item{node_id}{Original ID (can be character) for each node as per source dataframe.}

\item{node_number}{Numeric ID for each node - can be generated as per \code{make_connections()}.}

\item{connection_id}{Field to connect related nodes as per initial source data.}

\item{join_col}{Column (as character string) to join connections data back onto itself. Defaults to the \code{connection_id} field.}

\item{from_to_col}{Column (character string "from" or "to") use in determining which is from node and which is to node. You need to pre-define this column if you want to use the functionality. If not provided, which node is selected as from and to will depend on the \code{node_number}.}

\item{weight_col}{Column used to set edge weights. If not set, a count of unique connecting objects between nodes will be used.}

\item{weight_col_fun}{Function to apply to \code{weight_col()} data to get edges.}

\item{...}{Additional arguments to pass to \code{weight_col_fun()}.}
}
\value{
A dataframe containing details of vertices between nodes, including weights. Note this will have unique rows for each node-to-node combination.
}
\description{
Calculates weightings between nodes for a network diagram.
}
