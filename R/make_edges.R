#' Calculate vertices for network visualisations.
#'
#' Calculates weightings between nodes for a network diagram.
#'
#' @param connections_data data detailing connections between nodes
#' @param join_field field to join connections data back onto itself
#' @param node_id field which identifies nodes
#' @param node_number field identifying number for each node
#' @param connection_id field identifying connecting objects
#' @return A dataframe containing details of vertices between nodes, including weights.
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr transmute
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr left_join
#' @importFrom dplyr ungroup
#' @export

make_edges <- function(
  connections_data,
  node_id,
  node_number,
  connection_id,
  join_field = deparse(substitute(connection_id))
) {


    #consistent column  naming
    connections_data <- connections_data %>%
      rename(
        node_id_1 = {{node_id}},
        node_number = {{node_number}}
        )

connections_data %>%
  #join data back on to itself to get connections across projects
    left_join(
      connections_data %>%
        transmute(node_id_2 = node_id_1, {{connection_id}}),
      by = join_field
    ) %>%
  #remove 'self-connections'
    filter(node_id_1 != node_id_2) %>%
  #calculate weightings for each connection
    group_by(node_id_1, node_id_2) %>%
    summarise(weight = n()) %>%
    ungroup() %>%
  #bring back in numeric id to enable duplicate checking
    mutate(
      node_number_1 = match(node_id_1, connections_data$node_id_1) %>% connections_data$node_number[.],
      node_number_2 = match(node_id_2, connections_data$node_id_1) %>% connections_data$node_number[.]
    ) %>%
  #duplicate checking
  mutate(
    duplicate_check = paste(
      pmax(node_number_1, node_number_2),
      pmin(node_number_1, node_number_2)
    )
    ) %>%
  distinct(duplicate_check, .keep_all = T) %>%
  select(-duplicate_check, -node_number_1, -node_number_2)


}
