#' @import dbscan
#' @import ggplot2
#' @importFrom dbscan dbscan
#' @importFrom car ellipse
#' @importFrom graphics points locator plot
#' @importFrom grDevices palette
#' @importFrom stats na.omit 
#' @importFrom rjags jags.model jags.samples
#' @importFrom sn dmst extractSECdistr selm
#' @importFrom igraph max_cliques graph_from_adjacency_matrix
#' @importClassesFrom sn SECdistrMv
#' @importMethodsFrom sn plot
NULL

#' @name eightWayExampleData
#' @title Example data from an 8-parent MAGIC population. 
#' @docType data
#' @author Alex Whan, Matthew Morell, Rohan Shah, Colin Cavanagh
#' This is a sample of 200 genetic markers from an 8-way MAGIC population of 4229 lines.
NULL
