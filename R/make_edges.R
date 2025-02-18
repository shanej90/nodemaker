#' Calculate vertices for network visualisations.
#'
#' Calculates weightings between nodes for a network diagram.
#'
#' @param connections_data Dataframe detailing connections between nodes, as generated via `make_connections()`. This can have duplicate rows for node - connection object combinations, to account for how edge weighting should be handled.
#' @param node_id Original ID (can be character) for each node as per source dataframe.
#' @param node_number Numeric ID for each node - can be generated as per `make_connections()`.
#' @param connection_id Field to connect related nodes as per initial source data.
#' @param join_col Column (as character string) to join connections data back onto itself. Defaults to the `connection_id` field.
#' @param from_to_col Column (character string "from" or "to") use in determining which is from node and which is to node. You need to pre-define this column if you want to use the functionality. If not provided, which node is selected as from and to will depend on the `node_number`.
#' @param weight_col Column used to set edge weights. If not set, a count of unique connecting objects between nodes will be used.
#' @param weight_col_fun Function to apply to `weight_col()` data to get edges.
#' @param ... Additional arguments to pass to `weight_col_fun()`.
#' @return A dataframe containing details of vertices between nodes, including weights. Note this will have unique rows for each node-to-node combination.
#' @export

make_edges <- function(
  connections_data,
  node_id,
  node_number,
  connection_id,
  join_col = deparse(substitute(connection_id)),
  from_to_col = NULL,
  weight_col = NULL,
  weight_col_fun = NULL,
  ...
  ) {

  # capture columns as quosure if needed.
  weight_col_quo <- rlang::enquo(weight_col)
  weight_col_fun_quo <- rlang::enquo(weight_col_fun)
  from_to_col_quo <- rlang::enquo(from_to_col)

  #error checking
  if(!is.character(join_col) | length(join_col) > 1) {

    stop("`join_field` must be a single character string.")

  }

  if(
    (!rlang::quo_is_null(weight_col_quo) & rlang::quo_is_null(weight_col_fun_quo)) |
    (rlang::quo_is_null(weight_col_quo) & !rlang::quo_is_null(weight_col_fun_quo))
    ) {

    stop("You must use `weight_col` and `weight_col_fun` in combination.")

  }

  if(!rlang::quo_is_null(weight_col_quo)) {

    if(!is.numeric(dplyr::pull(connections_data, !!weight_col_quo))) {

    stop("`weight_col` must be numeric.")

    }

  }

  if(!rlang::quo_is_null(from_to_col_quo)) {

    from_to_values <- dplyr::pull(connections_data, {{from_to_col}})

    if(any(!from_to_values %in% c("from", "to"))) {

      stop("Only valid values for `from_to_col` are 'from' and 'to' (case sensitive).")

    }

  }


  #self join data...

  #...if from/to specified
  if(!rlang::quo_is_null(from_to_col_quo)) {

    from_df <- connections_data |>
      dplyr::rename(
        from = {{node_id}},
        node_number = {{node_number}}
        ) |>
      dplyr::filter({{from_to_col}} == "from")

    joined <- from_df |>
      dplyr::left_join(
        connections_data |>
          dplyr::filter({{from_to_col}} == "to") |>
          dplyr::transmute(to = {{node_id}}, {{connection_id}}),
        by = join_col
        ) |>
      dplyr::filter(from != to) |>
      dplyr::select(-{{from_to_col}})


  } else { #...if from/to not specified

    from_df <- connections_data |>
      dplyr::rename(
        from = {{node_id}},
        node_number = {{node_number}}
        )

    joined <-  from_df |>
      #join data back on to itself to get connections across projects
      dplyr::left_join(
        connections_data |>
          dplyr::transmute(to = {{node_id}}, {{connection_id}}),
        by = join_col
        ) |>
      #remove 'self-connections'
      dplyr::filter(from != to)

    }

  #calc weightings
  if(rlang::quo_is_null(weight_col_quo)) {

    weights <- joined |>
      dplyr::summarise(
        weight = dplyr::n_distinct({{connection_id}}),
        .by = c(from, to)
        )

  } else {

    weights <- joined |>
      dplyr::summarise(
        weight = weight_col_fun({{weight_col}}, ...),
        .by = c(from, to)
        )

  }

  #node id col name
  node_col <- connections_data |>
    dplyr::select({{node_id}}) |>
    colnames()

  #bring back in numeric id to enable duplicate checking
    weights |>
      dplyr::mutate(
        node_number_1 = lookup::vlookup(from, connections_data, node_col, "node_number"),
        node_number_2 = lookup::vlookup(to, connections_data, node_col, "node_number")
        ) |>
      #duplicate checking
      dplyr::mutate(
        duplicate_check = paste(
          pmax(node_number_1, node_number_2),
          pmin(node_number_1, node_number_2)
          )
        ) |>
      dplyr::distinct(duplicate_check, .keep_all = T) |>
      dplyr::select(-duplicate_check, -node_number_1, -node_number_2)


}
