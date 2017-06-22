#' @export
#' @importFrom rjags jags.model jags.samples
fitClusterModel <- function(data, startingPoints, n.iter)
{
  clusterCov <- cbind(c(0.005, 0), c(0, 0.1))/3
  N <- nrow(data)
  clust <- rep(NA,N) 
  dataList = list(
    y = data,
    N = N,
    clust = clust,
    priorMuParam = diag(2)*4,
    clusterCov = clusterCov,
    clusterCovConcentration = 30,
    errorMuParam = diag(2),
    errorCov = diag(2)*10/3,
    errorCovConcentration = 300,
    smallClusterCov = diag(2)*0.025/3,
    smallClusterCovConcentration = 1500
  )

  model.spec <- textConnection(model)
  inits <- lapply(1:length(startingPoints), function(x) list(muOfClust = rbind(startingPoints[[x]], c(NA, NA), c(0.5, 1))))
  jags <- jags.model(model.spec,
    data = dataList,
    n.chains=length(startingPoints),
    n.adapt=100, inits = inits)
  samples <- jags.samples(jags, variable.names = c("clust", "covInvMatrices", "muOfClust", "pClust"), n.iter = n.iter)
  result <- list(samples = samples, data = data)
  class(result) <- "magicHBC"
  return(result)
}

