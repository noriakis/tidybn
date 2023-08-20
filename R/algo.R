#' hc
#' 
#' Hill-Climbing algorithm using bnlearn() for tibble
#' 
#' 
#' @importFrom bnlearn hc
#'
#' @return
#' tbl_graph
#'
#'
#' @name hc
#'
#' @export
NULL

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
    return(tblg)
}


#' rsmax2
#' 
#' 2-phase Restricted Maximization (RSMAX2) hybrid algorithm for tibble
#' 
#' 
#' @importFrom bnlearn rsmax2
#'
#' @return
#' tbl_graph
#'
#'
#' @name rsmax2
#'
#' @export
NULL

#'
#' @export
rsmax2 <- function (...) 
{
    UseMethod("rsmax2")
}


#' @export
rsmax2.tbl_df <- function(x, whitelist = NULL, blacklist = NULL,
	restrict = "si.hiton.pc", maximize = "hc", restrict.args = list(),
	maximize.args = list(), debug = FALSE) {
	
    df <- x |> as.data.frame()
    res <- bnlearn::rsmax2(df, whitelist=whitelist, blacklist=blacklist,
                 restrict=restrict, maximize=maximize, restrict.args=restrict.args,
                 maximize.args=maximize.args, debug=debug)
    tblg <- res |> bnlearn::as.igraph() |> tidygraph::as_tbl_graph()
    
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
    return(tblg)
}