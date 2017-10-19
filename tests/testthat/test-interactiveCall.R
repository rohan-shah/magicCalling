context("Test interactiveCall function")

test_that("Testing first markers",
{
	data("eightWayExampleData", package="magicCalling")
	data <- eightWayExampleData[[1]]

	originalResult <- list(hasVariability = FALSE, data = data)
	class(originalResult) <- "markerResult"

	startingPointFunction <- function(data)
	{
		meanY <- mean(data[,2])
		startingPoints <- list(
			rbind(c(0.5, meanY), c(0.5, meanY)),
			rbind(c(0.5, meanY), c(0.5, meanY)),
			rbind(c(0.25, meanY), c(0.5, meanY)),
			rbind(c(0.25, meanY), c(0.5, meanY)),
			rbind(c(0.75, meanY), c(0.5, meanY)),
			rbind(c(0.75, meanY), c(0.5, meanY)),
			rbind(c(0.8, meanY), c(0.2, meanY)),
			rbind(c(0.8, meanY), c(0.2, meanY))
		)
	}

	#Commands are c (call) and then k (keep)
	interactiveCommands <- "c\nk"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have been marked as variable
	expect_equal(interactiveResult$hasVariability, TRUE)
	#Should have five different values - Two main alleles, hets, errors, and then uncalled. 
	expect_equal(length(unique(interactiveResult$classification)), 5L)
	#expect that cluster means are accurate using automatic calling
	expectedClusterMeans <- cbind(c(0.982, 0.647), c(1.483, 1.847))
	firstCheck <- all.equal(expectedClusterMeans, interactiveResult$clusterMeans[1:2, ], tolerance = 0.02, check.attributes = FALSE)
	secondCheck <- all.equal(expectedClusterMeans, interactiveResult$clusterMeans[2:1, ], tolerance = 0.02, check.attributes = FALSE)
	expect_true(isTRUE(firstCheck) || isTRUE(secondCheck))
	#Should have class markerResult
	expect_equal(class(interactiveResult), "markerResult")


	#Now use the locator function. 
	interactiveCommands <- "p\nk"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = list(), clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.9827357), y = c(1.826039, 1.441651))), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have been marked as variable
	expect_equal(interactiveResult$hasVariability, TRUE)
	#We should get out the correct cluster assignments. 
	firstCheck <- all.equal(expectedClusterMeans, interactiveResult$clusterMeans[1:2, ], tolerance = 0.02, check.attributes = FALSE)
	secondCheck <- all.equal(expectedClusterMeans, interactiveResult$clusterMeans[2:1, ], tolerance = 0.02, check.attributes = FALSE)
	expect_true(isTRUE(firstCheck) || isTRUE(secondCheck))


	#Test "d" for discard marker / no variability
	interactiveCommands <- "p\nd"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 60)), clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.6432759), y = c(1.826039, 1.826039))), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have been marked as NOT variable
	expect_equal(interactiveResult$hasVariability, FALSE)


	#Test o for back to original
	interactiveCommands <- "p\no\nk"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 60)), clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.6432759), y = c(1.826039, 1.826039))), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have been marked as NOT variable
	expect_equal(interactiveResult$hasVariability, FALSE)

	#Test DBSCAN
	dbscanParameters <- list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.01, minPts = 10))
	#Numbers four and three should do nothing. So this results in the original "No variability" result being returned. 
	interactiveCommands <- "4\n\n3\nk"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = dbscanParameters, clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.6432759), y = c(1.826039, 1.826039))), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have been marked as NOT variable
	expect_equal(interactiveResult$hasVariability, FALSE)

	#Test DBSCAN again
	interactiveCommands <- "p\n4\n\n2\nk"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = dbscanParameters, clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.6432759), y = c(1.826039, 1.826039))), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have 13 different groups
	expect_equal(length(unique(interactiveResult$classification)), 13L)

	#Test DBSCAN again
	interactiveCommands <- "p\n4\n\n1\nk"
	interactiveConnection <- textConnection(interactiveCommands)
	#Redirect graphics
	pdf(NULL)
		suppressWarnings(capture.output(interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = dbscanParameters, clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.6432759), y = c(1.826039, 1.826039))), runHeuristicsParameters = list(minHomozygoteSize = 200))))
	dev.off()
	#Should have three different groups - Two main alleles and then another group of "uncalled"
	expect_equal(length(unique(interactiveResult$classification)), 3L)
})
