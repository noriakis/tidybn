#' The functions are adapted from `tidygraph` to show BN relevant information
#' on tbl_graph object (under construction)

#' @importFrom tibble trunc_mat
#' @importFrom tools toTitleCase
#' @importFrom rlang as_quosure sym
#' @importFrom pillar style_subtle
#' @export
setClass("tbl_graph")
setClass("tbl_bn", contains="tbl_graph")

describe_graph_bn_prop <- function(x) {
    mb <- attr(x, "avg.mb") |> round(2)
    nbr <- attr(x, "avg.nbr") |> round(2)
    algo <- attr(x, "algo")

    desc <- c()
    desc[1] <- paste("Learning algorithm:", algo)
    desc[2] <- paste("Avg. markov blanket size:", mb)
    desc[3] <- paste("Avg. neighbourhood size:", nbr)
    if (!is.null(attr(x, "test"))) {
        desc[4] <- paste("Score:", attr(x, "test"))
    }
    desc
}

#' @importFrom igraph is_simple is_directed is_bipartite is_connected is_dag gorder
describe_graph_bn <- function(x) {
  if (gorder(x) == 0) return('An empty graph')
  prop <- list(simple = is_simple(x), directed = is_directed(x),
                  bipartite = is_bipartite(x), connected = is_connected(x),
                  tree = is_tree(x), forest = is_forest(x), DAG = is_dag(x))
  desc <- c()
  if (prop$tree || prop$forest) {
    desc[1] <- if (prop$directed) 'A rooted' else 'An unrooted'
    desc[2] <- if (prop$tree) 'tree' else paste0('forest with ', count_components(x), ' trees')
  } else {
    desc[1] <- if (prop$DAG) 'A directed acyclic' else if (prop$bipartite) 'A bipartite' else if (prop$directed) 'A directed' else 'An undirected'
    desc[2] <- if (prop$simple) 'simple graph' else 'multigraph'
    n_comp <- count_components(x)
    desc[3] <- paste0('with ' , n_comp, ' component', if (n_comp > 1) 's' else '')
  }
  paste(desc, collapse = ' ')
}
#' @importFrom igraph is_connected is_simple gorder gsize count_components is_directed
is_forest <- function(x) {
  !is_connected(x) && is_simple(x) && (gorder(x) - gsize(x) - count_components(x) == 0)
}
cat_subtle <- function(...) cat(pillar::style_subtle(paste0(...)))

print.tbl_bn <- function(x, ...) {
  arg_list <- list(...)
  arg_list[['useS4']] <- NULL
  graph_desc <- describe_graph_bn(x)
  not_active <- if (active(x) == 'nodes') 'edges' else 'nodes'
  top <- do.call(trunc_mat, modifyList(arg_list, list(x = as_tibble(x), n = 6)))
  top$summary[1] <- paste0(top$summary[1], ' (active)')
  names(top$summary)[1] <- toTitleCase(paste0(substr(active(x), 1, 4), ' data'))
  bottom <- do.call(trunc_mat, modifyList(arg_list, list(x = as_tibble(x, active = not_active), n = 3)))
  names(bottom$summary)[1] <- toTitleCase(paste0(substr(not_active, 1, 4), ' data'))
  cat_subtle('# A tbl_graph: ', gorder(x), ' nodes and ', gsize(x), ' edges\n', sep = '')
  cat_subtle('#\n')
  cat_subtle('# ', graph_desc, '\n', sep = '')
  cat_subtle('#\n')
  sapply(describe_graph_bn_prop(x), function(text) {
    cat_subtle('# ',text , '\n', sep = '')
  })
  cat_subtle('#\n')
  print(top)
  cat_subtle('#\n')
  print(bottom)
  invisible(x)
}
register_s3_method("tidybn", "print", "tbl_bn")