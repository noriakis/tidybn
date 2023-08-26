#' coefficientGraph
#' @export
coefficientGraph <- function(fitted) {

    coef_graph <- lapply(names(fitted), function(x) {
        coefs <- fitted[[x]]$coefficients 
        coefs_label <- names(coefs)
        coefs_label <- coefs_label[coefs_label!="(Intercept)"]
        new_edge <- lapply(coefs_label, function(y) {
            c(y, x, coefs[y] |> as.numeric())
        })
        do.call(rbind, new_edge)
    })
    coef_graph <- do.call(rbind, coef_graph) |>
        data.frame() |>
        `colnames<-`(c("from","to","coefficient")) |>
        mutate(coefficient=as.numeric(coefficient))
    coef_graph <- tbl_graph(edges=coef_graph)
    coef_graph
}

#' plotReducedDimWithGraph
#' @importFrom scater plotReducedDim
#' @export
plotReducedDimWithGraph <- function(graph, sce, dimred="PCA",
                                    colour_by="label",
                                    annotation="label",
                                    only_tbl_graph=FALSE,
                                    only_ggraph=FALSE, ...) {

    plrd <- scater::plotReducedDim(sce, dimred=dimred,
                                   scattermore=TRUE,
                                   colour_by=colour_by, ...)+
        theme(legend.position="none")
    meta <- sce |>
        colData() |>
        as.data.frame()

    raw_points <- cbind(plrd$data,
            meta[row.names(plrd$data), ]) |>
        mutate(annotation=!!sym(annotation))
    
    ## Obtain majority
    majority <- raw_points |>
        group_by(colour_by) |>
        count(annotation) |>
        summarise(max=annotation[which.max(n)])
    
    points <- raw_points |> group_by(colour_by) |>
        summarise(X=mean(X), Y=mean(Y))
    
    points <- left_join(majority, points) |>   
        rename(name=colour_by)
    
    gra_pos <- graph |> activate(nodes) |>
        left_join(points, by="name")
    if (only_tbl_graph) {return(gra_pos)}
    if (only_graph) {return(ggraph(gra_pos, layout="manual", x=X, y=Y))}
    plo <- ggraph(gra_pos, layout="manual", x=X, y=Y) + 
        geom_edge_diagonal( arrow=arrow(length=unit(1.5,"mm"), type="closed"))+
        scattermore::geom_scattermore(aes(x=X, y=Y, color=colour_by),
                                      data=raw_points, pointsize=2)+
        geom_node_text(
            aes(label=max), repel=TRUE, bg.colour="white",
            size=4, family="aleg"
        )+
        theme_void()+
        scale_edge_width(limits=c(0.5, 1), range=c(0.5, 1.5))+
        scale_edge_colour_continuous(limits=c(0.5, 1),
                                     low="steelblue", high="tomato")+
        theme(legend.position="none")
    return(plo)
}