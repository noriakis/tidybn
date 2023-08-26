#' @name hc
#' @rdname hc
#' @inherit bnlearn::hc
#'
#' @return A `tbl_graph` object.
#'
#' @examples
#' library(bnlearn)
#' hc(as_tibble(alarm))
#'
#' @export
hc <- function (...) 
{
    UseMethod("hc")
}

#' @export
hc.tbl_df <- function(x, start=NULL, whitelist=NULL,
                    blacklist=NULL, score=NULL, ...,
                    debug=FALSE, restart=0, perturb=1,
                    max.iter=Inf, maxp=Inf, optimized=TRUE) {
    df <- x |> as.data.frame()
    res <- bnlearn::hc(df, start=start, whitelist=whitelist, blacklist=blacklist,
                 score=score, debug=debug, restart=restart, perturb=perturb,
                 max.iter=max.iter, maxp=maxp, optimized=optimized)
    tblg <- res |> bnlearn::as.igraph() |> tidygraph::as_tbl_graph()
    
    ## Undirected arc (edge) is represented by multiple edges between two nodes
    ## Obtained from bnlearn print function
    params <- names(res$learning$args)
    nodes <- names(res$nodes)
    avg.mb <- mean(sapply(nodes, function(n) { length(res$nodes[[n]]$mb) }))
    avg.nbr <- mean(sapply(nodes, function(n) { length(res$nodes[[n]]$nbr) }))
    avg.ch <- mean(sapply(nodes, function(n) { length(res$nodes[[n]]$children) }))
    attr(tblg, "avg.mb") <- avg.mb 
    attr(tblg, "avg.nbr") <- avg.nbr 
    attr(tblg, "avg.ch") <- avg.ch
    for (at in res$learning |> names()) {
        attr(tblg, at) <- res$learning[[at]]
    }
    class(tblg) <- c("tbl_bn", class(tblg))

    return(tblg)
}


#' tabu
#' 
#' tabu algorithm using bnlearn() for tibble
#' 
#' @name tabu
#' @docType methods
#' @export
NULL

#' @export
tabu <- function (...) 
{
    UseMethod("tabu")
}
#' @export
tabu.tbl_df <- function (x, start=NULL, whitelist=NULL,
                                    blacklist=NULL, score=NULL, ...,
                                    debug=FALSE, tabu=10, max.tabu=tabu,
                                    max.iter=Inf, maxp=Inf, optimized=TRUE) {
    df <- x |> as.data.frame()
    res <- bnlearn::tabu(df, start=start, whitelist=whitelist, blacklist=blacklist,
              score=score, debug=debug, tabu=tabu, max.tabu=max.tabu,
              max.iter=max.iter, maxp=maxp, optimized=optimized)
    tblg <- res |> bnlearn::as.igraph() |> tidygraph::as_tbl_graph()
    
    ## Undirected arc (edge) is represented by multiple edges between two nodes
    ## Obtained from bnlearn print function
    params <- names(res$learning$args)
    nodes <- names(res$nodes)
    avg.mb <- mean(sapply(nodes, function(n) { length(res$nodes[[n]]$mb) }))
    avg.nbr <- mean(sapply(nodes, function(n) { length(res$nodes[[n]]$nbr) }))
    avg.ch <- mean(sapply(nodes, function(n) { length(res$nodes[[n]]$children) }))
    attr(tblg, "avg.mb") <- avg.mb 
    attr(tblg, "avg.nbr") <- avg.nbr 
    attr(tblg, "avg.ch") <- avg.ch
    for (at in res$learning |> names()) {
        attr(tblg, at) <- res$learning[[at]]
    }
    class(tblg) <- c("tbl_bn", class(tblg))

    return(tblg)
}



