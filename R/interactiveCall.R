#' @export
#' @title Interactively recall a marker
#' @description Interactively recall a marker, using DBSCAN or model-based clustering. 
#' @param originalResult The previous attempt at calling this marker. Must be an object of class \code{markerResult}. If no attempt has previously been made, a suitable value is \code{list(data = data, hasVariability = TRUE)}, with class set to \code{markerResult}; this specifies that in the previous calling result, this marker was determined to be monomorphic. 
#' @param startingPointFunction A function that generates a list of starting points for the model based clustering algorithm. Each starting point is a 2 x 2 matrix, containing two column vectors. These column vectors are the initial centers for the two homozygote clusters. 
#' @param n.iter The number of MCMC iterations to use for the model based clustering algorithm.
#' @param dbscanParameters A list with named entries. Each entry is it self a list with two entries, named \code{eps} and \code{minPts}; these entries are arguments to the DBSCAN algorithm. Each name of the list \code{dbscanParameters} is used as a hotkey in the interactive calling. See below for details. 
#' @param clusterModelParameters A list of parameters to the model based clustering algorithm. See \code{\link{fitClusterModel}} for further details. 
#' @param runHeuristicsParameters A list of parameters to the runHeuristics function. 
#' @param ... Undocumented arguments, used only for testing this function. 
#' @return An object of class \code{markerResult}, containing the details of the called marker.
#' 
#' @details
#' This function allows the user to interactively recall a marker using (primarily) keyboard commands. The current marker calling result is displayed in a plot. Once a command is entered, hitting ENTER/RETURN runs the command. The commands are as follows:
#' \describe{
#' 	\item{"k"}{Keep the current marker calling result and exit}
#'	\item{"d"}{Discard this maker, by returning a result indicating that this marker is momomorphic}
#'	\item{"o"}{Go back to the original result used as an input to this function}
#'	\item{"c"}{Generate a new set of marker calls using the model-based clustering algorithm, OR show the next model-based calling result. Once the last model-based calling result has been displayed, command "c" generates a new set of marker calls.}
#'	\item{"p"}{Apply the model-based clustering algorithm, but with the user selecting the initial centers of the two clusters by clicking on the plot.}
#' 	\item{other}{If the input command is a name of an entry of \code{dbscanParameters}, then the corresponding entry of \code{dbscanParameters} is used as the parameters for the DBSCAN algorithm.}
#' }
#' @examples
#' data("eightWayExampleData", package="magicCalling")
#' data <- eightWayExampleData[[1]]
#' #Original calling result marks the marker as monomorphic
#' originalResult <- list(hasVariability = FALSE, data = data)
#' class(originalResult) <- "markerResult"
#' #Specify the starting points for the model-based clustering algorithm. Four starting points, each specified twice. The y-coordinate is chosen as the mean of the y-coordinates of the input data. 
#' startingPointFunction <- function(data)
#' {
#'         meanY <- mean(data[,2])
#'         startingPoints <- list(
#'                 rbind(c(0.5, meanY), c(0.5, meanY)),
#'                 rbind(c(0.5, meanY), c(0.5, meanY)),
#'                 rbind(c(0.25, meanY), c(0.5, meanY)),
#'                 rbind(c(0.25, meanY), c(0.5, meanY)),
#'                 rbind(c(0.75, meanY), c(0.5, meanY)),
#'                 rbind(c(0.75, meanY), c(0.5, meanY)),
#'                 rbind(c(0.8, meanY), c(0.2, meanY)),
#'                 rbind(c(0.8, meanY), c(0.2, meanY))
#'         )
#' }
#' exampleDbscanParameters <- list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 105))
#' \dontrun{
#' #In this call, commands "1" and "2" run DBSCAN. Command "c" calls the model-based clustering algorithm, generating eight possible calls, and displays the first. The next seven commands "c" show the next model-based call. The eighth command "c" generates another eight possible calls and shows the first, etc. 
#' interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = exampleDbscanParameters, clusterModelParameters = magicCalling:::exampleModelParameters)
#' }
interactiveCall <- function(originalResult, startingPointFunction, n.iter, dbscanParameters, clusterModelParameters, runHeuristicsParameters, ...)
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
	result <- originalResult
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
			result <- originalResult
			plot(result)
			previousChain <- -1
		}
		#Recall
		else if(command == "c")
		{
			if(previousChain == -1 || previousChain == length(heuristicResults$classifications))
			{
				fittedModel <- do.call(fitClusterModel, c(list(data = data, startingPoints = startingPointFunction(data), n.iter = n.iter), clusterModelParameters))
				heuristicResults <- do.call(runHeuristics, c(list(fittedModel = fittedModel), runHeuristicsParameters))
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
			heuristicResults <- do.call(runHeuristics, c(list(fittedModel = fittedModel), runHeuristicsParameters))
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

