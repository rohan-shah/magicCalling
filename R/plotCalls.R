#' @export
plotCallResult <- function(result, allFounderNames, ...)
{
  founderIndices <- na.omit(match(allFounders, rownames(data)))
  data <- result$data
  if(result$hasVariability)
  {
    if("dbscan" %in% names(result) && result$dbscan)
    {
      result <- originalResult
      plot(data[-founderIndices,1], data[-founderIndices,2], col = result$classification[-founderIndices] + 1, pch = 16, ...)
      points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
    }
    else
    {
      founderIndices <- match(allFounders, rownames(data))
      plot(data[-founderIndices,1], data[-founderIndices,2], col = result$classification[-founderIndices], pch = 16, ...)
      ellipse(center = result$clusterMeans[1, ], shape = result$covariances[1,,], radius=5, col = 2)
      ellipse(center = result$clusterMeans[2, ], shape = result$covariances[2,,], radius=5, col = 3)
      ellipse(center = result$clusterMeans[3, ], shape = result$covariances[3,,], radius=5, col = 4)
      points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
    }
  }
  else
  {
    plot(data[,1], data[,2], col = 1, pch = 16, ...)
  }
}
