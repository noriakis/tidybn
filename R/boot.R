boot.strength <- function (...) 
{
    UseMethod("boot.strength")
}
#' @name boot.strength
#' @rdname boot.strength
#' @inherit bnlearn::boot.strength
#' 
#' @importFrom bnlearn boot.strength
#' @export
boot.strength.tbl_df <- function(data, cluster, R = 200, m = nrow(data),
  algorithm, algorithm.args = list(), cpdag = TRUE, debug = FALSE) {
    
    df <- data |> as.data.frame()
    bs <- bnlearn::boot.strength(df, cluster=cluster, R = R, m = m,
        algorithm=algorithm, algorithm.args = algorithm.args, cpdag = cpdag, debug = debug)
    tblg <- as_tbl_graph(bs)
    attributes(tblg)$method <- attributes(bs)$method
    attributes(tblg)$threshold <- attributes(bs)$threshold
    attributes(tblg)$nodes <- attributes(bs)$nodes
    class(tblg) <- c("tbl_bn", class(tblg))
    return(tblg)
}


#' @export
averaged.network <- function (...) 
{
    UseMethod("averaged.network")
}

averaged.network.tbl_graph <- function(tblg, threshold) {
    nds <- tblg |> activate(nodes) |> data.frame()
    raw_df <- tblg |> activate(edges) |> data.frame() |>
            mutate(from=nds[.data$from,"name"], to=nds[.data$to,"name"])
    conv_df <- raw_df[,c("from","to","strength")]
    class(conv_df) <- c(class(conv_df), "bn.strength")
    attributes(conv_df)$method <- attributes(tblg)$method 
    attributes(conv_df)$threshold <- attributes(tblg)$threshold 
    attributes(conv_df)$nodes <- attributes(tblg)$nodes 

    el <- bnlearn::averaged.network(conv_df, threshold=threshold) |> 
        bnlearn::as.igraph() |> igraph::as_edgelist() |>
      data.frame() |> `colnames<-`(c("from","to"))
    class(tblg) <- c("tbl_bn", class(tblg))
    return(tbl_graph(edges=merge(el, raw_df)))
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
register_s3_method("tidybn", "averaged.network", "tbl_graph")
