# nodemaker 0.0.1

* Switched all use of `magrittr` pipe (`%>%`) to native R pipe.
* Added functionality to help with plotting network diagrams.
* Added `weight_col` parameter to `make_connections()` so users can define a field to weight edges.
* Added `weight_col` and `weight_col_fun` parameters to `make_edges()` to allow users to define how to weight graph edges.
* In `make_edges()`, output dataframe now has `from` and `to` columns instead of `node_id_1` and `node_id_2`.
* Added `from_to_col` parameter in `make_edges()` to let users state which side of a connection is 'from' and which is 'to' when making dataframe.
* Added tests.
