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
      dplyr::rename(
        node_id_1 = {{node_id}},
        node_number = {{node_number}}
        )

connections_data %>%
  #join data back on to itself to get connections across projects
    dplyr::left_join(
      connections_data %>%
        dplyr::transmute(node_id_2 = node_id_1, {{connection_id}}),
      by = join_field
    ) %>%
  #remove 'self-connections'
    dplyr::filter(node_id_1 != node_id_2) %>%
  #calculate weightings for each connection
    dplyr::group_by(node_id_1, node_id_2) %>%
    dplyr::summarise(weight = dplyr::n()) %>%
    dplyr::ungroup() %>%
  #bring back in numeric id to enable duplicate checking
    dplyr::mutate(
      node_number_1 = match(node_id_1, connections_data$node_id_1) %>% connections_data$node_number[.],
      node_number_2 = match(node_id_2, connections_data$node_id_1) %>% connections_data$node_number[.]
    ) %>%
  #duplicate checking
  dplyr::mutate(
    duplicate_check = paste(
      pmax(node_number_1, node_number_2),
      pmin(node_number_1, node_number_2)
    )
    ) %>%
  dplyr::distinct(duplicate_check, .keep_all = T) %>%
  dplyr::select(-duplicate_check, -node_number_1, -node_number_2)


}
