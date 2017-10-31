#' @export
callFromMap <- function(rawData, thresholdChromosomes = 100, thresholdAlleleClusters = 1e-10, maxChromosomes = 2, existingImputations)
{
	rawResult <- addExtraMarkerFromRawCall(mpcrossMapped = existingImputations, newMarker = rawData)
	chromosomes <- names(existingImputations@map)
	chromosomeScores <- sapply(chromosomes, function(x) max(rawResult@data[names(rawResult@map[[x]])]))
	chromosomeScores <- sort(chromosomeScores, decreasing=TRUE)
	if(sum(chromosomeScores > thresholdChromosomes) > maxChromosomes) return(NULL)

	bestChromosomes <- names(chromosomeScores[chromosomeScores > thresholdChromosomes])
	bestPositionsChromosomes <- sapply(bestChromosomes, function(x) names(which.max(rawResult@data[names(rawResult@map[[x]])])))

	dataPerPosition <- clusters <- list()
	for(position in bestPositionsChromosomes)
	{
		dataPerPosition[[position]] <- factor(existingImputations@geneticData[[1]]@imputed@data[,position])
		results <- matrix(0, nrow = 8, ncol = 8)
		for(i in 1:7)
		{
			for(j in (i+1):8)
			{
				contrastRow <- rep(0, 8)
				contrastRow[i] <- 1
				contrastRow[j] <- -1
				contrasts(dataPerPosition[[position]]) <- cbind(condInterest = contrastRow)
				model <- lm(dataSubset ~ dataPerPosition[[position]])
				currentSummary <- summary(model)
				results[j, i] <- results[i, j] <- min(currentSummary[[1]]$coefficients[2, 4], currentSummary[[2]]$coefficients[2, 4])
			}
		}
		adjacencyMatrix <- matrix(0, nrow = 8, ncol = 8)
		threshold <- thresholdAlleleClusters
		adjacencyMatrix[results > threshold] <- 1
		diag(adjacencyMatrix) <- 1
		maxCliques <- igraph::max_cliques(igraph::graph_from_adjacency_matrix(adjacencyMatrix))
		#The max cliques should actually partition the graph, so check that
		maxCliqueVector <- do.call(c, maxCliques)
		if(length(maxCliqueVector) != 8 || length(unique(maxCliqueVector)) != 8) return(NULL)
		clusters[[position]] <- maxCliques
	}
	classifyPosition <- function(position)
	{
		factor(as.integer(dataPerPosition[[position]] %in% clusters[[position]][[1]]))
	}
	groups <- sapply(bestPositionsChromosomes, classifyPosition, simplify = FALSE)
	combinedGroups <- do.call(interaction, groups)
	nCombinedGroups <- length(levels(combinedGroups))
	levels(combinedGroups) <- as.character(1:nCombinedGroups)

	clusterAssignments <- vector(mode = "integer", length = nrow(rawData))
	clusterAssignments[] <- NA
	for(group in 1:nCombinedGroups)
	{
		groupData <- dataSubset[combinedGroups == group,]
		groupData <- as.data.frame(groupData)
		colnames(groupData) <- c("x", "y")
		model <- selm(cbind(x, y) ~ 1, family = "ST", fixed.param = list(alpha = 0), data = groupData)
		distribution <- extractSECdistr(model, compNames = c("x", "y"))

		pdf(NULL)
			plotResults <- plot(distribution, col = 2, probs = 0.5, landmarks = "")
		dev.off()
		data <- cbind(plotResults$plot$contourLines[[1]]$x, plotResults$plot$contourLines[[1]]$y)
		contourValue <- mean(sn:::dmst(data, dp = plotResults$object@dp))

		isInside <- sn:::dmst(rawData, dp = plotResults$object@dp) > contourValue
		clusterAssignments[isInside] <- group
	}
	return(clusterAssignments)
}
