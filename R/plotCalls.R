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
  ellipse1 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
  ellipse2 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
  ellipse3 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
  if(missing(allFounderNames))
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), row.names = rownames(data))
    basePlot <- ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour"), data = data) + ggplot2::geom_point(shape = 16)
    built <- ggplot2::ggplot_build(basePlot)
    y.range <- built$layout$panel_ranges[[1]]$y.range
    x.range <- built$layout$panel_ranges[[1]]$x.range
    basePlot + ggplot2::geom_path(data = ellipse1, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 2) + ggplot2::geom_path(data = ellipse2, mapping = ggplot2::aes(x = "x", y = "y"), colour = 3) + ggplot2::geom_path(data = ellipse3, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 4) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + ggplot2::scale_shape_identity() + coord_cartesian(xlim = x.range, ylim = y.range, expand = FALSE)
  }
  else
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), shape = 16, row.names = rownames(data))
    founderIndices <- na.omit(match(allFounderNames, rownames(data)))
    data[founderIndices, "shape"] <- 2
    data[founderIndices, "colour"] <- 1
    #Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
    founderIndicesSubset <- data[data[,"shape"] == 2, ]
    basePlot <-  ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour", shape = "shape"), data = data) + ggplot2::geom_point() 
    built <- ggplot2::ggplot_build(basePlot)
    y.range <- built$layout$panel_ranges[[1]]$y.range
    x.range <- built$layout$panel_ranges[[1]]$x.range
    basePlot + ggplot2::geom_path(data = ellipse1, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 2) + ggplot2::geom_path(data = ellipse2, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 3) + ggplot2::geom_path(data = ellipse3, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 4) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + ggplot2::scale_shape_identity() + ggplot2::geom_point(data = founderIndicesSubset) + coord_cartesian(xlim = x.range, ylim = y.range, expand = FALSE)
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
  ellipse1 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][1, ], shape = heuristicResults$covariances[[chainIndex]][1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
  ellipse2 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][2, ], shape = heuristicResults$covariances[[chainIndex]][2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
  ellipse3 <- as.data.frame(ellipse(center = heuristicResults$clusterMeans[[chainIndex]][3, ], shape = heuristicResults$covariances[[chainIndex]][3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
  if(missing(allFounderNames))
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), row.names = rownames(data))
    basePlot <- ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour"), data = data) + ggplot2::geom_point(shape = 16) 
    built <- ggplot2::ggplot_build(basePlot)
    y.range <- built$layout$panel_ranges[[1]]$y.range
    x.range <- built$layout$panel_ranges[[1]]$x.range
    basePlot + ggplot2::geom_path(data = ellipse1, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 2) + ggplot2::geom_path(data = ellipse2, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 3) + ggplot2::geom_path(data = ellipse3, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 4) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + ggplot2::scale_shape_identity() + coord_cartesian(xlim = x.range, ylim = y.range, expand = FALSE)
  }
  else
  {
    data <- data.frame(x = data[,1], y = data[,2], colour = factor(heuristicResults$classification[[chainIndex]], levels = 1:5), shape = 16, row.names = rownames(data))
    founderIndices <- na.omit(match(allFounderNames, rownames(data)))
    data[founderIndices, "shape"] <- 2
    data[founderIndices, "colour"] <- 1
    #Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
    founderIndicesSubset <- data[data[,"shape"] == 2,]
    basePlot <- ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour", shape = "shape"), data = data) + ggplot2::geom_point() 
    built <- ggplot2::ggplot_build(basePlot)
    y.range <- built$layout$panel_ranges[[1]]$y.range
    x.range <- built$layout$panel_ranges[[1]]$x.range
    basePlot + ggplot2::geom_path(data = ellipse1, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 2) + ggplot2::geom_path(data = ellipse2, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 3) + ggplot2::geom_path(data = ellipse3, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 4) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + ggplot2::geom_point(data = founderIndicesSubset) + ggplot2::scale_shape_identity() + coord_cartesian(xlim = x.range, ylim = y.range, expand = FALSE)
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
          ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour"), data = data) + ggplot2::geom_point(shape = 16) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE)
	}
	else
	{
          founderIndices <- na.omit(match(allFounderNames, rownames(data)))
          data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification+1, levels = 1:(max(x$classification)+1)), shape = 16, row.names = rownames(data))
	  data[founderIndices, "shape"] <- 2
	  data[founderIndices, "colour"] <- 1
          #Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
          founderIndicesSubset <- data[data[,"shape"] == 2,]
          ggplot2::ggplot(mapping = ggplot2::aes(x = "x", y = "y", colour = "colour", shape = "shape"), data = data) + ggplot2::geom_point() + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + ggplot2::geom_point(data = founderIndicesSubset) + ggplot2::scale_shape_identity()
	}
    }
    else
    {
      ellipse1 <- as.data.frame(ellipse(center = x$clusterMeans[1, ], shape = x$covariances[1,,], radius=5, col = 2, center.pch = NULL, draw = FALSE))
      ellipse2 <- as.data.frame(ellipse(center = x$clusterMeans[2, ], shape = x$covariances[2,,], radius=5, col = 3, center.pch = NULL, draw = FALSE))
      ellipse3 <- as.data.frame(ellipse(center = x$clusterMeans[3, ], shape = x$covariances[3,,], radius=5, col = 4, center.pch = NULL, draw = FALSE))
      if(missing(allFounderNames))
      {
        data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification, levels = 1:5), row.names = rownames(data))
        basePlot <- ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour"), data = data) + ggplot2::geom_point(shape = 16) 
        built <- ggplot2::ggplot_build(basePlot)
        y.range <- built$layout$panel_ranges[[1]]$y.range
        x.range <- built$layout$panel_ranges[[1]]$x.range
	basePlot + ggplot2::geom_path(data = ellipse1, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 2) + ggplot2::geom_path(data = ellipse2, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 3) + ggplot2::geom_path(data = ellipse3, mapping = ggplot2::aes_string(x = "x", y = "y"), colour = 4) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + coord_cartesian(xlim = x.range, ylim = y.range, expand = FALSE)
      }
      else
      {
        data <- data.frame(x = data[,1], y = data[,2], colour = factor(x$classification, levels = 1:5), shape = 16, row.names = rownames(data))
        founderIndices <- na.omit(match(allFounderNames, rownames(data)))
	data[founderIndices, "shape"] <- 2
	data[founderIndices, "colour"] <- 1
	#Split out the founder indices subset, because these will have different plot symbols, which we want to appear *on top* of everything else.
	founderIndicesSubset <- data[data[,"shape"] == 2,]
        basePlot <- ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour"), data = data) + ggplot2::geom_point(mapping = ggplot2::aes_string(x = "x", y = "y", colour = "colour", shape = "shape")) + ggplot2::scale_shape_identity()
        built <- ggplot2::ggplot_build(basePlot)
        y.range <- built$layout$panel_ranges[[1]]$y.range
        x.range <- built$layout$panel_ranges[[1]]$x.range
	basePlot + ggplot2::geom_path(data = ellipse1, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 2) + ggplot2::geom_path(data = ellipse2, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 3) + ggplot2::geom_path(data = ellipse3, mapping = ggplot2::aes_string(x = "x", y = "y", shape = NULL), colour = 4) + ggplot2::theme_bw() + ggplot2::scale_color_manual(values = palette(), guide = FALSE) + ggplot2::geom_point(data = founderIndicesSubset) + coord_cartesian(xlim = x.range, ylim = y.range, expand = FALSE)
      }
    }
  }
  else
  {
    data <- data.frame(x = data[,1], y = data[,2], row.names = rownames(data))
    ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y"), data = data) + ggplot2::geom_point(colour = 1, shape = 16) + ggplot2::scale_shape_identity()
  }
}
