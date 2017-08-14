#' @export
interactiveCall <- function(originalResult, data, markerName, allFounders, startingPointFunction, n.iter, dbscanParameters, clusterModelParamaters, ...)
{
	if(class(originalResult) != "markerResult")
	{
		stop("Input originalResult must have class \"markerResult\"")
	}
	plot(originalResult)
	previousChain <- -1
	while(TRUE)
	{
		command <- readLines(n=1)
		#Keep
		if(command == "k")
		{
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
			plot(originalResult)
			previousChain <- -1
		}
		#Recall
		else if(command == "c")
		{
			if(previousChain == -1 || previousChain == length(heuristicResults$classifications))
			{
				fittedModel <- do.call(fitClusterModel, c(list(data = data, startingPoints = startingPointFunction(data), n.iter = n.iter), clusterModelParameters))
				heuristicResults <- runHeuristics(fittedModel)
				previousChain <- 1
			}
			else previousChain <- previousChain + 1
			plot(heuristicResults, chainIndex = previousChain)
			result <- list(hasVariability = TRUE, classifications = heuristicResults$classifications[[previousChain]], clusterMeans = heuristicResults$clusterMeans[[previousChain]], covariances = heuristicResults$covariances[[previousChain]], data = data, dbscan = FALSE)
			class(result) <- "markerResult"
		}
		#Specify starting points
		else if(command == "p")
		{
			previousChain <- -1
			points <- locator(n = 2)
			fittedModel <- do.call(fitClusterModel, c(list(data = data, startingPoints = list(cbind(points$x, points$y)), n.iter = n.iter), clusterModelParameters))
			heuristicResults <- runHeuristics(fittedModel)
			plot(heuristicResults)
			result <- list(hasVariability = TRUE, classifications = heuristicResults$classifications[[1]], clusterMeans = heuristicResults$clusterMeans[[1]], covariances = heuristicResults$covariances[[1]], data = data, dbscan = FALSE)
			class(result) <- "markerResult"
		}
		#DBScan 
		else if(command %in% names(dbscanParameters))
		{
			currentDBScanParameters <- dbscanParameters[[command]]
			dbscanResult <- dbscan(data, eps = currentDBScanParameters$eps, minPts = currentDBScanParameters$minPts)
			result <- list(hasVariability = TRUE, classification = dbscanResult$cluster, data = data, dbscan = TRUE)
			names(result$classification) <- rownames(data)
			class(result) <- "markerResult"
			plot(result)
		}
	}
}

