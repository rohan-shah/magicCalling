context("Test fitting of model")

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
		interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 200, dbscanParameters = list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 60)), clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)))
	dev.off()
	
	#Should have been marked as variable
	expect_equal(interactiveResult$hasVariability, TRUE)

	#expect that cluster means are accurate using automatic calling
	expectedClusterMeans <- cbind(c(0.982, 0.647), c(1.483, 1.847))
	expect_true(all.equal(expectedClusterMeans, interactiveResult$clusterMeans[1:2, ], tolerance = 0.01, check.attributes = FALSE) || all.equal(expectedClusterMeans, interactiveResult$clusterMeans[2:1, ], tolerance = 0.01, check.attributes = FALSE))

	#Should have class markerResult
	expect_equal(class(interactiveResult), "markerResult")


	#Now use the locator function. 
	interactiveCommands <- "p\nk"
	interactiveConnection <- textConnection(interactiveCommands)

	#Redirect graphics
	pdf(NULL)
		interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 100, dbscanParameters = list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 60)), clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.9827357, 0.9827357), y = c(1.441651, 1.441651))))
	dev.off()

	#Should have been marked as variable
	expect_equal(interactiveResult$hasVariability, TRUE)

	#Both cluster means should be around 0.98 still, clusters should be located at the same place. 
	expect_gt(interactiveResult$clusterMeans[1,1], 0.85)
	expect_gt(interactiveResult$clusterMeans[2,1], 0.85)

	#Use the locator function again, but with both clusters starting on the left-most cluster. 
	interactiveCommands <- "p\nk"
	interactiveConnection <- textConnection(interactiveCommands)

	#Redirect graphics
	pdf(NULL)
		interactiveResult <- interactiveCall(originalResult, startingPointFunction = startingPointFunction, n.iter = 100, dbscanParameters = list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 60)), clusterModelParameters = magicCalling:::exampleModelParameters, readLinesFunction = function() return(readLines(interactiveConnection, n = 1)), locatorFunction = function() return(list(x = c(0.6432759, 0.6432759), y = c(1.826039, 1.826039))))
	dev.off()

	#Should have been marked as variable
	expect_equal(interactiveResult$hasVariability, TRUE)

	#Both cluster means should be around 0.98 still, clusters should be located at the same place. 
	expect_lt(interactiveResult$clusterMeans[1,1], 0.85)
	expect_lt(interactiveResult$clusterMeans[2,1], 0.85)
})
