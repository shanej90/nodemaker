#' Reformat dataframes in an appropriate format for network analysis.
#'
#' Take a data frame and create a dataframe of unique nodes containing their dataframe unique ids, and a 'node id' as well.
#'
#' @param attribute_data Dataframe containing node attribute data
#' @param node_col Column name in \code{attribute_data} that contains the unique common node identifier
#' @param ... Attribute variables in \code{attribute_data}. Note the function assumes there is only one value for each attribute per node.
#' @return A dataframe of nodes as per \code{node_col}
#' @export


make_nodes <- function(
  attribute_data,
  node_col,
  ...
  ) {

  attribute_data |>
    dplyr::select({{node_col}}, ...) |>
    dplyr::distinct({{node_col}}, .keep_all = T) |>
    tibble::rowid_to_column("id")
}
