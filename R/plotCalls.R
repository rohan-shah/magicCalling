#' @export 
plot.hierarchicalBayesianModel <- function(x, allFounderNames, chainIndex, ...)
{
  heuristicResults <- runHeuristics(x)
  if(missing(chainIndex) && length(heuristicResults$classification) == 1)
  {
    chainIndex <- 1
  }
  if(missing(chainIndex))
  {
    stop("Input chainIndex is required, if there are multiple Markov chains")
  }
  data <- heuristicResults$data
  if(missing(allFounderNames))
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), row.names = rownames(data))
    ellipse1 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
    ellipse2 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
    ellipse3 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
    ggplot(mapping = aes(x, y, colour = colour), data = data) + geom_point(shape = 16) + geom_path(data = ellipse1, mapping = aes(x, y), colour = 2) + geom_path(data = ellipse2, mapping = aes(x, y), colour = 3) + geom_path(data = ellipse3, mapping = aes(x, y), colour = 4) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE) + scale_shape_identity()
  }
  else
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), shape = 16, row.names = rownames(data))
    founderIndices <- na.omit(match(allFounderNames, rownames(data)))
    data[founderIndices, "shape"] <- 2
    data[founderIndices, "colour"] <- 1
    #Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
    founderIndicesSubset <- data[data[,"shape"] == 2,]
    ellipse1 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
    ellipse2 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
    ellipse3 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
    ggplot(mapping = aes(x, y, colour = colour, shape = shape), data = data) + geom_point() + geom_path(data = ellipse1, mapping = aes(x, y, shape = NULL), colour = 2) + geom_path(data = ellipse2, mapping = aes(x, y, shape = NULL), colour = 3) + geom_path(data = ellipse3, mapping = aes(x, y, shape = NULL), colour = 4) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE) + scale_shape_identity() + geom_point(data = founderIndicesSubset)
  }
}
#' @export 
plot.magicHeuristicHBC <- function(x, allFounderNames, chainIndex, ...)
{
  if(missing(chainIndex) && length(x$classification) == 1)
  {
    chainIndex <- 1
  }
  if(missing(chainIndex))
  {
    stop("Input chainIndex is required, if there are multiple Markov chains")
  }
  heuristicResults <- x
  data <- heuristicResults$data
  if(missing(allFounderNames))
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), row.names = rownames(data))
    ellipse1 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
    ellipse2 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
    ellipse3 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
    ggplot(mapping = aes(x, y, colour = colour), data = data) + geom_point(shape = 16) + geom_path(data = ellipse1, mapping = aes(x, y), colour = 2) + geom_path(data = ellipse2, mapping = aes(x, y), colour = 3) + geom_path(data = ellipse3, mapping = aes(x, y), colour = 4) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE) + scale_shape_identity()
  }
  else
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), shape = 16, row.names = rownames(data))
    founderIndices <- na.omit(match(allFounderNames, rownames(data)))
    data[founderIndices, "shape"] <- 2
    data[founderIndices, "colour"] <- 1
    #Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
    founderIndicesSubset <- data[data[,"shape"] == 2,]
    ellipse1 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
    ellipse2 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
    ellipse3 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
    ggplot(mapping = aes(x, y, colour = colour, shape = shape), data = data) + geom_point() + geom_path(data = ellipse1, mapping = aes(x, y, shape = NULL), colour = 2) + geom_path(data = ellipse2, mapping = aes(x, y, shape = NULL), colour = 3) + geom_path(data = ellipse3, mapping = aes(x, y, shape = NULL), colour = 4) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE) + geom_point(data = founderIndicesSubset) + scale_shape_identity()
  }
}
#' @export 
plot.markerResult <- function(x, allFounderNames, ...)
{
  data <- x$data
  if(x$hasVariability)
  {
    if("dbscan" %in% names(x) && x$dbscan)
    {
    	if(missing(allFounderNames))
	{
          data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification + 1, levels = 1:(max(x$classification)+1)))
          ggplot(mapping = aes(x, y, colour = colour), data = data) + geom_point(shape = 16) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE)
	}
	else
	{
          founderIndices <- na.omit(match(allFounderNames, rownames(data)))
          data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification+1, levels = 1:(max(x$classification)+1)), shape = 16, row.names = rownames(data))
	  data[founderIndices, "shape"] <- 2
	  data[founderIndices, "colour"] <- 1
          #Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
          founderIndicesSubset <- data[data[,"shape"] == 2,]
          ggplot(mapping = aes(x, y, colour = colour, shape = shape), data = data) + geom_point() + theme_bw() + scale_color_manual(values = palette(), guide = FALSE) + geom_point(data = founderIndicesSubset) + scale_shape_identity()
	}
    }
    else
    {
      if(missing(allFounderNames))
      {
        data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification, levels = 1:5), row.names = rownames(data))
        ellipse1 <- as.data.frame(ellipse(center = x$clusterMeans[1, ], shape = x$covariances[1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
        ellipse2 <- as.data.frame(ellipse(center = x$clusterMeans[2, ], shape = x$covariances[2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
        ellipse3 <- as.data.frame(ellipse(center = x$clusterMeans[3, ], shape = x$covariances[3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
        ggplot(mapping = aes(x, y, colour = colour), data = data) + geom_point(shape = 16) + geom_path(data = ellipse1, mapping = aes(x, y), colour = 2) + geom_path(data = ellipse2, mapping = aes(x, y), colour = 3) + geom_path(data = ellipse3, mapping = aes(x, y), colour = 4) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE)
      }
      else
      {
        data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification, levels = 1:5), shape = 16, row.names = rownames(data))
        founderIndices <- na.omit(match(allFounderNames, rownames(data)))
	data[founderIndices, "shape"] <- 2
	data[founderIndices, "colour"] <- 1
	#Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
	founderIndicesSubset <- data[data[,"shape"] == 2,]
        ellipse1 <- as.data.frame(ellipse(center = x$clusterMeans[1, ], shape = x$covariances[1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
        ellipse2 <- as.data.frame(ellipse(center = x$clusterMeans[2, ], shape = x$covariances[2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
        ellipse3 <- as.data.frame(ellipse(center = x$clusterMeans[3, ], shape = x$covariances[3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
        ggplot(mapping = aes(x, y, colour = colour, shape = shape), data = data) + geom_point() + geom_path(data = ellipse1, mapping = aes(x, y, shape = NULL), colour = 2) + geom_path(data = ellipse2, mapping = aes(x, y, shape = NULL), colour = 3) + geom_path(data = ellipse3, mapping = aes(x, y, shape = NULL), colour = 4) + theme_bw() + scale_color_manual(values = palette(), guide = FALSE) + scale_shape_identity() + geom_point(data = founderIndicesSubset)
      }
    }
  }
  else
  {
    data <- data.frame(x = data[,1], y = data[,2], row.names = rownames(data))
    ggplot(mapping = aes(x, y), data = data) + geom_point(colour = 1, shape = 16) + scale_shape_identity()
  }
}
