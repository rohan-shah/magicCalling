#' @export
interactiveCall <- function(originalResult, startingPointFunction, n.iter, dbscanParameters, clusterModelParameters, ...)
{
	extraArgs <- list(...)
	if(!("locatorFunction" %in% names(extraArgs)))
	{
		extraArgs$locatorFunction <- function()
		{
			return(locator(n = 2))
		}
	}
	if(!("readLinesFunction" %in% names(extraArgs)))
	{
		extraArgs$readLinesFunction <- function()
		{
			return(readLines(n = 1))
		}
	}
	if(class(originalResult) != "markerResult")
	{
		stop("Input originalResult must have class \"markerResult\"")
	}
	data <- originalResult$data
	plot(originalResult)
	previousChain <- -1
	while(TRUE)
	{
		command <- extraArgs$readLinesFunction()
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
			#Replaced locator(n = 2) with something that can be overriden, for the purposes of testing. 
			points <- extraArgs$locatorFunction()
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

