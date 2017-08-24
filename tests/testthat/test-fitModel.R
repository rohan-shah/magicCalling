context("Test fitting of model")

test_that("Testing first marker",
{
	data("eightWayExampleData", package="magicCalling")
	
	#Test using eight starting points
	data <- eightWayExampleData[[1]]
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
	capture.output(clusterModel <- do.call(fitClusterModel, c(list(data = data, startingPoints = startingPoints, n.iter = 200), magicCalling:::exampleModelParameters)))
	#Result has correct class
	expect_equal(class(clusterModel), "hierarchicalBayesianModel")
	
	#Test using four starting points. 
	startingPoints <- list(
		rbind(c(0.5, meanY), c(0.5, meanY)),
		rbind(c(0.25, meanY), c(0.5, meanY)),
		rbind(c(0.75, meanY), c(0.5, meanY)),
		rbind(c(0.8, meanY), c(0.2, meanY))
	)
	capture.output(clusterModel <- do.call(fitClusterModel, c(list(data = data, startingPoints = startingPoints, n.iter = 200), magicCalling:::exampleModelParameters)))
	#Result has correct class
	expect_equal(class(clusterModel), "hierarchicalBayesianModel")
})
