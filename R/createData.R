#' Function used to generate the example data. 
createData <- function(rawDataPartsDir)
{
	wd <- getwd()
	setwd("~/8way_map")
	source("./constants.R")
	partFile <- file.path(rawDataPartsDir, "dataChunk1.RData")
	load(partFile)
	newMarkerNames <- paste0("Marker ", 1:nrow(thetaChunk))
	newLineNames <- paste0("Line ", 1:ncol(thetaChunk))

	rownames(thetaChunk) <- rownames(rValuesChunk) <- newMarkerNames
	colnames(thetaChunk) <- colnames(rValuesChunk) <- newLineNames

	nMarkers <- 200
	eightWayExampleData <- lapply(as.list(1:200), function(x)
		{
			cbind("Theta" = unlist(thetaChunk[x,]), "r" = unlist(rValuesChunk[x,]))
		})
	setwd(wd)
	save(eightWayExampleData, file = "eightWayExampleData.RData")
}
