#' @export
#' @title Select best fit of the hierarchical Bayesian cluster model
#' @description Use heuristics to select the best-fitting hierarchical Bayesian cluster model, from multiple fitted models. Certain data processing is performed using the best fitting model, to generate a cluster assignment variable. 
#' @param fittedModel An object of class \code{hierarchicalBayesianModel}, containing data about one or more fitted models. 
#' @param minHomozygoteSize The minimum allowable size of the homyzoget clusters. 
#' @return An object of class \code{magicHeuristicHBC}, which only contains data about the best-fitting model. 
#' 
#' @details
#' This function takes as input multiple fitted Hierarchical Bayesian Cluster models. These models are fitted using MCMC, often using different initial states. This function produces a summary of the fitted modles, including cluster assignment variables and component covariance matrices.  It also chooses a `best' fitted model. 
#'
#' Selecting the `best' fitted model is complicated. We begin by rejecting models for which any of the following are true:
#' \describe{
#' 	\item{1.}{For more than 5\% of the data points, the posterior probability of membership is more than 15\%, for BOTH homozygote clusters}
#' 	\item{2.}{Either of the homozygote clusters is smaller than minHomozygoteSize}
#' 	\item{3.}{The diffeence in the x-coordinate of the two homozygote clusters is smaller than 0.06}
#'	\item{4.}{More than a quater of the points fall into the outlier cluster}
#' 	\item{5.}{The difference in the centers of the homozygote clusters is small, relative to the covariance matrix of those clusters}
#' }
#' If there are any remaining models, then for each model we compute the determinants of the covariance matrices of the heterozygote clusters. We select the model for this value (the maximum determinant) is smallest. 
#'
#' If there are no remaining models, then the returned object indicates that no suitable model was found. 
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
