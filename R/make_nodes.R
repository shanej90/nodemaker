#' Reformat dataframes in an appropriate format for network analysis.
#'
#' Take a data frame and create a df of unique nodes containing their df unique ids, and a 'node id' as well.
#'
#' @param attribute_data Dataframe containing node attribute data
#' @param node_col Column name in \code{attribute_data} that contains the unique common node identifier
#' @param ... Attribute variables in \code{attribute_data}
#' @return A df of nodes as per \code{node_col}
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr distinct
#' @export


make_nodes <- function(
  attribute_data,
  node_col,
  ...
) {

  attribute_data %>%
    select({{node_col}}, ...) %>%
    distinct({{node_col}}, .keep_all = T) %>%
    rowid_to_column("id")
}
