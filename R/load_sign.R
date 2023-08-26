#' loadSign
#'
#' Load the output of SiGN-BN (HC+BS)
#'
#' @param fileName the result of SiGN-BN
#' @param return_tbl_graph return tbl_graph
#'
#' @return list of edges, nodes, strength, and bn (bnlearn)
#' @examples loadSign("result.txt")
#' @export
#'
loadSign <- function(fileName, return_tbl_graph=TRUE){
  returnList <- list()
  rawStr <- readChar(fileName, file.info(fileName)$size)
  edges <- read.csv(text=unlist(strsplit(rawStr, "\\[Edges]\n"))[2], sep="\t", header=F)
  nodes <- read.csv(text=unlist(strsplit(unlist(strsplit(rawStr, "\\[Edges]\n"))[1], "\\[Nodes]\n"))[2], sep="\t", header=F)
  
  changeName <- list()
  for (i in seq_len(dim(nodes)[1])){
    changeName[[as.character(nodes[i,]$V3)]] <- nodes[i,]$V1
  }
  
  edges$V1 <- sapply(edges$V1, function(x) changeName[[as.character(x)]])
  edges$V2 <- sapply(edges$V2, function(x) changeName[[as.character(x)]])
  
  signStr <- edges[,c(1:3)]
  colnames(signStr) <- c("from","to","strength")
  convertStr <- edges[,c(1:3,5,7)]
  colnames(convertStr) <- c("from","to","strength","direction","updown")

  attr(signStr, "nodes") <- unique(c(signStr$from, signStr$to))
  signStr <- structure(signStr, method = "bootstrap", threshold = 0,
    class = c("bn.strength", class(signStr)))
  
  if (return_tbl_graph) {
    tblg <- as_tbl_graph(convertStr)
    attributes(tblg)$method <- attributes(signStr)$method
    attributes(tblg)$threshold <- bnlearn::inclusion.threshold(signStr)
    attributes(tblg)$nodes <- attributes(signStr)$nodes
    class(tblg) <- c("tbl_bn", class(tblg))
    return(tblg)
  } else {
    returnList[["str"]] <- signStr
    returnList[["edges"]] <- edges
    returnList[["nodes"]] <- nodes
    signBn <- bnlearn::averaged.network(signStr, threshold=bnlearn::inclusion.threshold(signStr))
    returnList[["av"]] <- signBn
    return(returnList)    
  }
}