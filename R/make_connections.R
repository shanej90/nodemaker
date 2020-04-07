#' Create a dataframe listing nodes and details of how they connect to other nodes
#'
#' Creates a df of unique nodes and vertices combinations. For example, it might list all team members/nodes working on projects, connected via a project ID.
#'
#' @param data Dataframe holding the information you'd like to reformat
#' @param node_col The \code{data} column containing identifiers for nodes
#' @param connect_col The \code{data} column containing the ids of objects connecting nodes, for example, project IDs, locations, mentees
#' @return A dataframe containign details of connections between nodes via 'connecting objects'.
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr row_number
#' @export

make_connections <- function(
  data,
  node_col,
  connect_col
) {

  data %>%
    distinct({{node_col}}, {{connect_col}}) %>%
    group_by({{node_col}}) %>%
    mutate(node_number = row_number()) %>%
    ungroup()
}
