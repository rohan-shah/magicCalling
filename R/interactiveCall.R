interactiveCall <- function(originalResult, data, allFounders, startingPoints, n.iter, dbscanParameters, ...)
{
	if(originalResult$hasVariability)
	{
		classification <- originalResult$classification
		founderIndices <- na.omit(match(allFounders, rownames(data)))
		if("dbscan" %in% names(originalResult) && originalResult$dbscan)
		{
			result <- originalResult
			plot(data[-founderIndices,1], data[-founderIndices,2], col = classification[-founderIndices] + 1, pch = 16, ...)
			points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
		}
		else
		{
			plot(data[-founderIndices,1], data[-founderIndices,2], col = classification[-founderIndices], pch = 16, ...)
			ellipse(center = originalResult$clusterMeans[1, ], shape = originalResult$covariances[1,,], radius=5, col = 2)
			ellipse(center = originalResult$clusterMeans[2, ], shape = originalResult$covariances[2,,], radius=5, col = 3)
			ellipse(center = originalResult$clusterMeans[3, ], shape = originalResult$covariances[3,,], radius=5, col = 4)
			points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)

			result <- originalResult
			result$dbscan <- FALSE
		}
	}
	else
	{
		founderIndices <- na.omit(match(allFounders, rownames(data)))
		originalResult$data <- data
		originalResult$dbscan <- FALSE
		result <- originalResult
		plot(data[,1], data[,2], pch = 16, main = markerName, ...)
	}
	previousChain <- length(startingPoints)
	while(TRUE)
	{
		command <- readLines(n=1)
		#Keep
		if(command == "k")
		{
			class(result) <- "markerResult"
			return(result)
		}
		#Discard this marker
		else if(command == "d")
		{
			result <- list(hasVariability = FALSE)
			class(result) <- "markerResult"
			return(result)
		}
		#Back to original
		else if(command == "o")
		{
			result <- originalResult
			if(result$hasVariability)
			{
				plot(data[-founderIndices,1], data[-founderIndices,2], col = classification[-founderIndices], pch = 16, main = markerName)
				ellipse(center = originalResult$clusterMeans[1,], shape = originalResult$covariances[1,,], radius=5, col = 2)
				ellipse(center = originalResult$clusterMeans[2,], shape = originalResult$covariances[2,,], radius=5, col = 3)
				ellipse(center = originalResult$clusterMeans[3,], shape = originalResult$covariances[3,,], radius=5, col = 4)
				points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)

				result$dbscan <- FALSE
			}
			else
			{
				plot(data[,1], data[,2], pch = 16, main = markerName)
			}
			previousChain <- length(startingPoints)
		}
		#Recall
		else if(command == "c")
		{
			if(previousChain == 6)
			{
				fittedModel <- fitModel(data, startingPoints = startingPoints, n.iter = n.iter)
				heuristicResults <- runHeuristics(fittedModel)
				previousChain <- 1
			}
			else previousChain <- previousChain + 1
			plot(data[-founderIndices,1], data[-founderIndices,2], col = heuristicResults$classifications[[previousChain]][-founderIndices], pch = 16, ...)
			ellipse(center = heuristicResults$clusterMeans[[previousChain]][1, ], shape = heuristicResults$covariances[[previousChain]][1,,], radius=5, col = 2)
			ellipse(center = heuristicResults$clusterMeans[[previousChain]][2, ], shape = heuristicResults$covariances[[previousChain]][2,,], radius=5, col = 3)
			ellipse(center = heuristicResults$clusterMeans[[previousChain]][3, ], shape = heuristicResults$covariances[[previousChain]][3,,], radius=5, col = 4)
			points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
			result <- list(hasVariability = TRUE, classifications = heuristicResults$classifications[[previousChain]], clusterMeans = heuristicResults$clusterMeans[[previousChain]], covariances = heuristicResults$covariances[[previousChain]], data = data, dbscan = FALSE)
		}
		#Specify starting points
		else if(command == "p")
		{
			previousChain <- 6
			points <- locator(n = 2)
			fittedModel <- fitModel(data, startingPoints = list(cbind(points$x, points$y)))
			heuristicResults <- runHeuristics(fittedModel)
			plot(data[-founderIndices,1], data[-founderIndices,2], col = heuristicResults$classifications[[1]][-founderIndices], pch = 16, ...)
			ellipse(center = heuristicResults$clusterMeans[[1]][1, ], shape = heuristicResults$covariances[[1]][1,,], radius=5, col = 2)
			ellipse(center = heuristicResults$clusterMeans[[1]][2, ], shape = heuristicResults$covariances[[1]][2,,], radius=5, col = 3)
			ellipse(center = heuristicResults$clusterMeans[[1]][3, ], shape = heuristicResults$covariances[[1]][3,,], radius=5, col = 4)
			points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
			result <- list(hasVariability = TRUE, classifications = heuristicResults$classifications[[1]], clusterMeans = heuristicResults$clusterMeans[[1]], covariances = heuristicResults$covariances[[1]], data = data, dbscan = FALSE)
			
		}
		#DBScan 
		else if(command %in% names(dbscanParameters))
		{
			currentDBScanParameters <- dbscanParameters[[command]]
			dbscanResult <- dbscan(data, eps = currentDBScanParameters$eps, minPts = currentDBScanParameters$minPts)
			plot(data[-founderIndices, 1], data[-founderIndices,2], col = dbscanResult$cluster[-founderIndices]+1, pch = 16, main = markerName)
			points(data[founderIndices,1], data[founderIndices,2], pch = 2, col = 1)
			result <- list(hasVariability = TRUE, classification = dbscanResult$cluster, data = data, dbscan = TRUE)
			names(result$classification) <- rownames(data)
		}
	}
}

