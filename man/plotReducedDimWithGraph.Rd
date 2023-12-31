% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotReducedDimWithGraph.R
\name{plotReducedDimWithGraph}
\alias{plotReducedDimWithGraph}
\title{plotReducedDimWithGraph}
\usage{
plotReducedDimWithGraph(
  graph,
  sce,
  dimred = "PCA",
  colour_by = "label",
  annotation = "label",
  only_tbl_graph = FALSE,
  edge_color = "grey",
  only_ggraph = FALSE,
  ...
)
}
\arguments{
\item{colour_by}{scatter color in reduced dimension plot,
should match the name of graph, otherwise output error}
}
\description{
plotReducedDimWithGraph
}
