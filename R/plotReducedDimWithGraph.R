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
#' @param colour_by scatter color in reduced dimension plot,
#' should match the name of graph, otherwise output error
#' @importFrom scater plotReducedDim
#' @export
plotReducedDimWithGraph <- function(graph, sce, dimred="PCA",
    colour_by="label", annotation="label", only_tbl_graph=FALSE,
    edge_color="grey", only_ggraph=FALSE, ...) {

    plrd <- scater::plotReducedDim(sce, dimred=dimred,
                                   scattermore=TRUE,
                                   colour_by=colour_by, ...) +
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
    
    if (intersect(points$name,
    	graph |> activate(nodes) |> pull(name)) |> length() == 0) {
    	stop("No matching name")
    }
    
        
    gra_pos <- graph |> activate(nodes) |>
        left_join(points, by="name")
    if (only_tbl_graph) {return(gra_pos)}
    if (only_ggraph) {return(ggraph(gra_pos, layout="manual", x=X, y=Y))}
    
    plo <- ggraph(gra_pos, layout="manual", x=X, y=Y) + 
        geom_edge_diagonal(arrow=arrow(length=unit(1.5,"mm"), type="closed"),
        	color=edge_color)+
        scattermore::geom_scattermore(aes(x=X, y=Y, color=colour_by),
                                      data=raw_points, pointsize=2)+
        geom_node_text(
            aes(label=max), repel=TRUE, bg.colour="white",
            size=4,
        )+
        theme_void()+
        scale_edge_width(limits=c(0.5, 1), range=c(0.5, 1.5))+
        scale_edge_colour_continuous(limits=c(0.5, 1),
                                     low="steelblue", high="tomato")+
        theme(legend.position="none")
    return(plo)
}


#' countEdge
#' count edge number based on cluster label and annotation
#' @export
countEdge <- function(graph, sce, cluster="label",
                      annotation="label") {
    
    raw_points <- colData(sce) |>
        as.data.frame() |>
        mutate(annotation=!!sym(annotation))
    
    ## Obtain majority
    majority <- raw_points |>
        group_by(!!sym(cluster)) |>
        count(annotation) |>
        summarise(max=annotation[which.max(n)])
    
    ## Mapper
    name_mapper <- majority$max |> setNames(majority$label)
    
    gra_pos <- graph |> activate(nodes) |>
        mutate(annotation=name_mapper[name])
    node_names <- gra_pos |> activate(nodes) |> data.frame()
    
    gra_pos_ed <- gra_pos |> 
        activate(edges) |> data.frame()
    
    annot_count <- table(raw_points$annotation)

    tibble(
        node_names[gra_pos_ed$from, "annotation"],
        node_names[gra_pos_ed$to, "annotation"]
    ) |> `colnames<-`(c("from","to")) |>
        mutate(edge=paste0(from," -> ",to),
               from_c=annot_count[from],
               to_c=annot_count[to]) |>
        group_by(edge) |>
        summarise(n=n(), from_c=unique(from_c),
                  to_c=unique(to_c)) |>
        arrange(desc(n))
}