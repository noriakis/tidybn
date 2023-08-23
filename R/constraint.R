#' @export
pc.stable <- function (...) 
{
    UseMethod("pc.stable")
}

#' @export
pc.stable.tbl_df <- function(x, cluster, whitelist = NULL, blacklist = NULL, test = NULL,
  alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE, undirected = FALSE) {
    df <- x |> as.data.frame()
    res <- bnlearn::pc.stable(df, cluster=cluster, whitelist=whitelist,
    	        blacklist=blacklist,
    	        test=test,
    	        alpha=alpha,
    	        B=B,
    	        max.sx=max.sx,
    	        debug=debug,
    	        undirected=undirected)

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


#' @export
gs <- function (...) 
{
    UseMethod("gs")
}

#' @importFrom bnlearn gs
#' @export
gs.tbl_df <- function(x, cluster, whitelist = NULL, blacklist = NULL, test = NULL,
  alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE, undirected = FALSE) {
	df <- x |> as.data.frame()
    res <- bnlearn::gs(df,
    	cluster=cluster,
    	whitelist=whitelist,
        blacklist=blacklist,
        test=test,
        alpha=alpha,
        B=B,
        max.sx=max.sx,
        debug=debug,
        undirected=undirected)

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


#' @export
iamb <- function (...) 
{
    UseMethod("iamb")
}

#' @importFrom bnlearn iamb
#' @export
iamb.tbl_df <- function(x, cluster, whitelist = NULL, blacklist = NULL, test = NULL,
  alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE, undirected = FALSE) {
	df <- x |> as.data.frame()
    res <- bnlearn::gs(df,
    	cluster=cluster,
    	whitelist=whitelist,
        blacklist=blacklist,
        test=test,
        alpha=alpha,
        B=B,
        max.sx=max.sx,
        debug=debug,
        undirected=undirected)

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
