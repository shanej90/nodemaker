#' Reformat dataframes in an appropriate format for network analysis.
#'
#' Take a data frame and create a df of unique nodes containing their df unique ids, and a 'node id' as well.
#'
#' @param connection_data Dataframe holding the information you'd like to reformat
#' @param connection_node_col The field containing the node ids/values in \code{connection_data}
#' @param attribute_data Dataframe containing node attribute data
#' @param attribute_node_col Column name in \code{attribute_data} that contains the unique common node identifier
#' @param ... Attribute variables in \code{attribute_data}
#' @return A df of nodes as per \code{node_col}
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @export


make_nodes <- function(
  connection_data,
  connection_node_col,
  attribute_data,
  attribute_node_col,
  ... = NULL
) {

  if(!is.null(...))

  connection_data %>%
    rename(node_id = {{connection_node_col}}) %>%
    distinct(node_id) %>%
    rowid_to_column("id") %>%
    left_join(
      attribute_data %>%
        rename(node_id = {{attribute_node_col}}) %>%
        select(node_id, ...),
      by = "node_id"
    )

  else

    connection_data %>%
    rename(node_id = {{connection_node_col}}) %>%
    distinct(node_id) %>%
    rowid_to_column("id")

}
