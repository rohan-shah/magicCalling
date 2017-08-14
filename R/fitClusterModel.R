#' @export
#' @importFrom rjags jags.model jags.samples
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
  class(result) <- "magicHBC"
  return(result)
}

