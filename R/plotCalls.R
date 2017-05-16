#' @export 
plot.magicHBC <- function(x, allFounderNames, chainIndex, ...)
{
  heuristicResults <- runHeuristics(x)
  data <- heuristicResults$data
  founderIndices <- na.omit(match(allFounderNames, rownames(data)))

  plot(data[-founderIndices,1], data[-founderIndices,2], col = heuristicResults$classification[[chainIndex]][-founderIndices], pch = 16, ...)
  ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL)
  ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL)
  ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL)
  points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
}
#' @export 
plot.magicHeuristic <- function(x, allFounderNames, ...)
{
  heuristicResults <- x
  data <- heuristicResults$data
  founderIndices <- na.omit(match(allFounderNames, rownames(data)))
  if(result$hasVariability)
  {
    if("dbscan" %in% names(result) && result$dbscan)
    {
        plot(data[-founderIndices,1], data[-founderIndices,2], col = result$classification[-founderIndices] + 1, pch = 16, ...)
        points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
    }
    else
    {
      plot(data[-founderIndices,1], data[-founderIndices,2], col = heuristicResults$classification[-founderIndices], pch = 16, ...)
      ellipse(center = heuristicResults$clusterMeans[1, ], shape = heuristicResults$covariances[1,,], radius=5, col = 2, center.pch = NULL)
      ellipse(center = heuristicResults$clusterMeans[2, ], shape = heuristicResults$covariances[2,,], radius=5, col = 3, center.pch = NULL)
      ellipse(center = heuristicResults$clusterMeans[3, ], shape = heuristicResults$covariances[3,,], radius=5, col = 4, center.pch = NULL)
      points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
    }
  }
  else
  {
    plot(data[,1], data[,2], col = 1, pch = 16, ...)
  }
}
