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
plot.magicHeuristicHBC <- function(x, allFounderNames, chainIndex, ...)
{
  heuristicResults <- x
  data <- heuristicResults$data
  founderIndices <- na.omit(match(allFounderNames, rownames(data)))
  plot(data[-founderIndices,1], data[-founderIndices,2], col = heuristicResults$classification[[chainIndex]][-founderIndices], pch = 16, ...)
  ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL)
  ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL)
  ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL)
  points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
}
#' @export 
plot.markerResult <- function(x, allFounderNames, ...)
{
  data <- x$data
  founderIndices <- na.omit(match(allFounderNames, rownames(data)))
  if(x$hasVariability)
  {
    if("dbscan" %in% names(x) && x$dbscan)
    {
        plot(data[-founderIndices,1], data[-founderIndices,2], col = x$classification[-founderIndices] + 1, pch = 16, ...)
        points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
    }
    else
    {
      plot(data[-founderIndices,1], data[-founderIndices,2], col = x$classification[-founderIndices], pch = 16, ...)
      ellipse(center = x$clusterMeans[1, ], shape = x$covariances[1,,], radius=5, col = 2, center.pch = NULL)
      ellipse(center = x$clusterMeans[2, ], shape = x$covariances[2,,], radius=5, col = 3, center.pch = NULL)
      ellipse(center = x$clusterMeans[3, ], shape = x$covariances[3,,], radius=5, col = 4, center.pch = NULL)
      points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
    }
  }
  else
  {
    plot(data[,1], data[,2], col = 1, pch = 16, ...)
  }
}
