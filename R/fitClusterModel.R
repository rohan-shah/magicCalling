#' @export
#' @title Perform model-based clustering
#' @description Perform model-based clustering, using a normal mixture model that includes heterozygotes. 
#' @param data The input data, as a two-column matrix
#' @param startingPoints A list of starting points for the clustering algorithm. Each starting point is a 2 x 2 matrix, containing two column vectors. These column vectors are the initial centers for the two homozygote clusters.
#' @param n.iter The number of MCMC iterations to use for the clustering algorithm.
#' @param D_hom 2 x 2 covariance matrix controlling the distribution of the *mean* of each homozygote cluster. It is sensible to use a multiple of the identity matrix. 
#' @param V_hom 2 x 2 covariance matrix controlling the distribution of the homozygous alleles *around the cluster mean*. The value of this parameter depends on the shapes of the clusters corresponding to homzygous alleles. The value given in \code{magicCalling:::exampleModelParameters} corresponds to clusters that are axis-aligned, with the clusters being larger alogn the y-axis than the x-axis. 
#' @param n_hom Parameter specifying the degree of certainty about the shapes of the homozygote clusters. If this parameter is very large, than the shape of the homozygous clusters is completely determined by \code{V_hom}. If it is small, then the shapes of the clusters will be determined more by the data than by \code{V_hom}.  
#' @param D_err 2 x 2 covariance matrix controlling the distribution of the *mean* of the error cluster. It is sensible to use a multiple of the identity matrix.
#' @param V_err 2 x 2 covariance matrix controlling the distribution of the errors *around the cluster mean*. It is sensible to use a (large) multiple of the identity matrix, indicating a high detgree of uncertainty. 
#' @param n_err Parameter specifying the degree of certainty about the shape of the error cluster. This parameter should be small, indicating a high degree of uncertainty. 
#' @param V_het 2 x 2 covariance matrix controlling the distribution of the heterozygotes around the *mean* of the cluster. It is sensible to use a small multiple of the identity matrix, so that this cluster tends to be small. 
#' @param n_het Parameter specifying the degree of certainty about the shape of the heterozygote cluster. This parameter should be large, indicating a high degree of certainty that that the heterozygote cluster is small. 
#' @details This function applies a model-based clustering method. The data is assumed to be generated by a four-component normal distribution, consisting of two clusters of homozygous alleles, a cluster of heterozygotes lying exactly between the two homozygotes, and a cluster containing errors. This model is fit using the JAGS software package. 
#' @examples
#' data("eightWayExampleData", package="magicCalling")
#' data <- eightWayExampleData[[1]]
#' meanY <- mean(data[,2])
#' startingPoints <- list(
#' 	rbind(c(0.5, meanY), c(0.5, meanY)),
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
#' plot(result, chainIndex = 2)
fitClusterModel <- function(data, startingPoints, n.iter, D_hom, V_hom, n_hom, D_err, V_err, n_err, V_het, n_het)
{
  N <- nrow(data)
  clust <- rep(NA,N) 
  dataList = list(
    y = data,
    N = N,
    clust = clust,
    D_hom = D_hom,
    V_hom = V_hom,
    n_hom = n_hom,
    D_err = D_err,
    V_err = V_err,
    n_err = n_err,
    V_het = V_het,
    n_het = n_het
  )

  model.spec <- textConnection(model)
  inits <- lapply(1:length(startingPoints), function(x) list(muOfClust = rbind(startingPoints[[x]], c(NA, NA), c(0.5, 1))))
  jags <- jags.model(model.spec,
    data = dataList,
    n.chains=length(startingPoints),
    n.adapt=100, inits = inits)
  samples <- jags.samples(jags, variable.names = c("clust", "covInvMatrices", "muOfClust", "pClust"), n.iter = n.iter)
  result <- list(samples = samples, data = data)
  class(result) <- "hierarchicalBayesianModel"
  return(result)
}
