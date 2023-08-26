
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
    class(tblg) <- c("tbl_bn", class(tblg))

    return(tblg)
}


#'
#' @export
mmhc <- function (...) 
{
    UseMethod("mmhc")
}

#' @export
mmhc.tbl_df <- function(x, whitelist = NULL, blacklist = NULL, restrict.args = list(),
  maximize.args = list(), debug = FALSE) {
    df <- x |> as.data.frame()
    res <- bnlearn::mmhc(df, whitelist=whitelist, blacklist=blacklist,
                 restrict.args=restrict.args,
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
    class(tblg) <- c("tbl_bn", class(tblg))

    return(tblg)    
}


#' @export
mmtabu <- function(...) {
	UseMethod("mmtabu")
}

#' @export
mmtabu.tbl_df <- function(x, whitelist = NULL, blacklist = NULL, restrict.args = list(),
  maximize.args = list(), debug = FALSE) {
    df <- x |> as.data.frame()
    restrict <- "mmpc"; maximize="tabu";
    res <- bnlearn::rsmax2(df, whitelist=whitelist, blacklist=blacklist,
    	restrict=restrict, maximize=maximize,
        restrict.args=restrict.args,
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
    class(tblg) <- c("tbl_bn", class(tblg))

    return(tblg)    
}


#'
#' @export
h2pc <- function (...) 
{
    UseMethod("h2pc")
}

#' @export
h2pc.tbl_df <- function(x, whitelist = NULL, blacklist = NULL, restrict.args = list(),
  maximize.args = list(), debug = FALSE) {
    df <- x |> as.data.frame()
    res <- bnlearn::h2pc(df, whitelist=whitelist, blacklist=blacklist,
                 restrict.args=restrict.args,
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
    class(tblg) <- c("tbl_bn", class(tblg))

    return(tblg)       
}