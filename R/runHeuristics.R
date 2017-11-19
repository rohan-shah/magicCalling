#' @export
#' @title Select best fit of the hierarchical Bayesian cluster model
#' @description Use heuristics to select the best-fitting hierarchical Bayesian cluster model, from multiple fitted models. Certain data processing is performed using the best fitting model, to generate a cluster assignment variable. 
#' @param fittedModel An object of class \code{hierarchicalBayesianModel}, containing data about one or more fitted models. 
#' @param minHomozygoteSize The minimum allowable size of the homzygote clusters. 
#' 
#' @details
#' This function takes as input multiple fitted Hierarchical Bayesian Cluster models. These models are fitted using MCMC, often using different initial states. This function produces a summary of the fitted models, including cluster assignment variables and component covariance matrices.  It also chooses a `best' fitted model. 
#'
#' Selecting the `best' fitted model is complicated. We begin by rejecting models for which any of the following are true:
#' \describe{
#' 	\item{1.}{For more than 5\% of the data points, the posterior probability of membership is more than 15\%, for BOTH homozygote clusters}
#' 	\item{2.}{Either of the homozygote clusters is smaller than minHomozygoteSize}
#' 	\item{3.}{The difference in the x-coordinate of the two homozygote clusters is smaller than 0.06}
#'	\item{4.}{More than a quater of the points fall into the outlier cluster}
#' 	\item{5.}{The difference in the centers of the homozygote clusters is small, relative to the covariance matrix of those clusters}
#' }
#' If there are any remaining models, then for each model we compute the maximum of the determinants of the covariance matrices of the homozygote clusters. We select the model for this value (the maximum determinant) is smallest. 
#'
#' If there are no remaining models, then the returned object indicates that no suitable model was found. 
#' @return An object of class \code{magicHeuristicHBC}, containing data about all the fitted models, and an indication of which model is best, according to the heuristics described in this section. 
#'   \item{classifications}{A list with an entry for each Markov chain. Each entry is a vector of cluster assignments for the data points.}
#'   \item{clusterMeans}{A list with an entry for each Markov chain. Each entry is a matrix with three rows and two columns. Each row gives the center point of a cluster. The first two rows are the centers of the homozygote clusters, and the third row is the center of the heterozygote cluster.}
#'   \item{covariances}{A list with an entry for each Markov chain. Each entry is a 4 x 2 x 2 matrix, so that \code{m[i, , ]} is a 2 x 2 matrix representing the covariance matrix of a component. The first two such matrices represent the covariance matrices of the homozygote clusters. The third represents the covariance matrix of the homozygote cluster. The fourth represents the covariance matrix of the error component.}
#'   \item{chainIndex}{The index of the `best' Markov chain. If no chain satisfied the required conditions, then this is set to -1. }
#'   \item{data}{The input data.}
#' @examples
#' data("eightWayExampleData", package="magicCalling")
#' data <- eightWayExampleData[[1]]
#' meanY <- mean(data[,2])
#' startingPoints <- list(
#'  rbind(c(0.5, meanY), c(0.5, meanY)),
#'      rbind(c(0.5, meanY), c(0.5, meanY)),
#'      rbind(c(0.25, meanY), c(0.5, meanY)),
#'      rbind(c(0.25, meanY), c(0.5, meanY)),
#'      rbind(c(0.75, meanY), c(0.5, meanY)),
#'      rbind(c(0.75, meanY), c(0.5, meanY)),
#'      rbind(c(0.8, meanY), c(0.2, meanY)),
#'      rbind(c(0.8, meanY), c(0.2, meanY))
#' )
#' result <- fitClusterModel(data, startingPoints, n.iter = 200, D_hom = diag(2)*4, V_hom = cbind(c(0.005, 0), c(0, 0.1))/3, n_hom = 30, D_err = diag(2), V_err = diag(2)*10/3, n_err = 300, V_het = diag(2)*0.025/3, n_het = 1500)
#' plot(result, chainIndex = 1)
#' heuristicResults <- runHeuristics(result, minHomozygoteSize = 200)
#' plot(heuristicResults, chainIndex = heuristicResults$chainIndex)
runHeuristics <- function(fittedModel, minHomozygoteSize)
{
  N <- nrow(fittedModel$data)
  samples <- fittedModel$samples
  validIndices <- c()
  maxDets <- c()
  classifications <- clusterMeans <- covariances <- list()
  n.chains <- dim(samples$clust)[3]
  n.iter <- dim(samples$clust)[2]
  for(chain in 1:n.chains)
  {
    cluster1Counts <- apply(samples$clust[,,chain], 1, function(x) sum(x == 1))
    cluster2Counts <- apply(samples$clust[,,chain], 1, function(x) sum(x == 2))
    cluster3Counts <- apply(samples$clust[,,chain], 1, function(x) sum(x == 3))
    cluster4Counts <- apply(samples$clust[,,chain], 1, function(x) sum(x == 4))
  
    #Reject models where there is too much overlap between the two main clusters in terms of calling. Or either of the main clusters is too small. Or the centres are too close in the x-axis. Or there are too many outliers. Or the ellipses are too close (one contains the centre of the other) 
    quadratic1 <- (samples$muOfClust[1,,n.iter, chain] - samples$muOfClust[2,,n.iter, chain]) %*% samples$covInvMatrices[1,,,n.iter, chain] %*% (samples$muOfClust[1,,n.iter, chain] - samples$muOfClust[2,,n.iter, chain])
    quadratic2 <- (samples$muOfClust[1,,n.iter, chain] - samples$muOfClust[2,,n.iter, chain]) %*% samples$covInvMatrices[2,,,n.iter, chain] %*% (samples$muOfClust[1,,n.iter, chain] - samples$muOfClust[2,,n.iter, chain])
    maxDetsThisChain <- max(det(solve(samples$covInvMatrices[1, , , n.iter, chain])), det(solve(samples$covInvMatrices[2, , , n.iter, chain])))
    if(sum(cluster1Counts > n.iter * 0.15 & cluster2Counts > n.iter * 0.15) > 0.05 * N || sum(cluster1Counts > n.iter*0.5) < minHomozygoteSize || sum(cluster2Counts > n.iter*0.5) < minHomozygoteSize || abs(samples$muOfClust[2,1,n.iter,chain] - samples$muOfClust[1,1,n.iter,chain]) < 0.06 || sum(cluster4Counts > n.iter * 0.5) > 0.25 * N || quadratic1 < 25 || quadratic2 < 25 || maxDetsThisChain > 4e-4)
    {
    } else
    {
      validIndices <- c(validIndices, chain)
      maxDets <- c(maxDets, maxDetsThisChain)
    }
    currentClassification <- vector(mode = "integer", length = N)
    currentClassification[] <- 1
    currentClassification[cluster1Counts > n.iter * 0.5] <- 2
    currentClassification[cluster2Counts > n.iter * 0.5] <- 3
    currentClassification[cluster3Counts > n.iter * 0.9] <- 4
    currentClassification[cluster4Counts > n.iter * 0.5] <- 5

    minClusterTheta <- min(samples$muOfClust[1,1,n.iter,chain], samples$muOfClust[2,1,n.iter,chain])
    maxClusterTheta <- max(samples$muOfClust[1,1,n.iter,chain], samples$muOfClust[2,1,n.iter,chain])

    #Remove hets which are not in between the two main clusters. 
    currentClassification[currentClassification == 4 & ((fittedModel$data[,1] < 0.15 * maxClusterTheta + 0.85 * minClusterTheta) | (fittedModel$data[,1] > 0.85 * maxClusterTheta + 0.15 * minClusterTheta))] <- 1
    names(currentClassification) <- rownames(fittedModel$data)

    classifications[[chain]] <- currentClassification

    currentCovariances <- samples$covInvMatrices[1:4, , , n.iter, chain]
    for(i in 1:4) currentCovariances[i, ,] <- solve(currentCovariances[i, ,])
    covariances[[chain]] <- currentCovariances

    clusterMeans[[chain]] <- samples$muOfClust[1:3, , n.iter, chain]
  }
  if(length(validIndices) > 0)
  {
    chainIndex <- validIndices[which.min(maxDets)]
  } 
  else chainIndex <- -1
  result <- list(classifications = classifications, clusterMeans = clusterMeans, covariances = covariances, chainIndex = chainIndex, data = fittedModel$data)
  class(result) <- "magicHeuristicHBC"
  return(result)
}
