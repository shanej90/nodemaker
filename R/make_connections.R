#' Create a dataframe listing nodes and details of how they connect to other nodes
#'
#' Creates a df of unique nodes and vertices combinations. For example, it might list all team members/nodes working on projects, connected via a project ID.
#'
#' @param data Dataframe holding the information you'd like to reformat
#' @param node_col The \code{data} column containing identifiers for nodes
#' @param connect_col The \code{data} column containing the ids of objects connecting nodes, for example, project IDs, locations, mentees
#' @param weight_col A column from `data` containing quantitative information to set edge width for connections in `make_edges()`. Optional.
#' @return A dataframe containing details of connections between nodes via 'connecting objects'. Will include a unique `node_number` column for use in downstream functions. Note this dataframe can have duplicate node-connecting object rows depending on the nature of the source dataframe.
#' @export

make_connections <- function(
  data,
  node_col,
  connect_col,
  weight_col = NULL
  ) {

  #error checking
  if(!is.numeric(data |> dplyr::pull({{weight_col}}))) {

    stop("`weight_col` must be numeric.")

  }

  data |>
    dplyr::select({{node_col}}, {{connect_col}}, {{weight_col}}) |>
    dplyr::mutate(node_number = match({{node_col}}, unique({{node_col}})))
}
