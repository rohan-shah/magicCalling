model <- "model {
    # Likelihood:
    for( i in 1 : N ) {
      y[i, 1:2] ~ dmnorm( mu[i,] , covObs[i,1:2, 1:2]) 
      mu[i, 1:2] <- muOfClust[clust[i], ]
      covObs[i, 1:2, 1:2] <- covInvMatrices[clust[i], 1:2, 1:2]
      clust[i] ~ dcat( pClust[1:4] )
    }
    # Prior:
    covInvMatrices[1,1:2,1:2] ~ dwish(V_hom*n_hom, n_hom)
    covInvMatrices[2,1:2,1:2] ~ dwish(V_hom*n_hom, n_hom)
    covInvMatrices[3,1:2,1:2] ~ dwish(V_het*n_het, n_het)
    covInvMatrices[4,1:2,1:2] ~ dwish(V_err*n_err, n_err)

    muOfClust[1,1:2] ~ dmnorm(c(0.25, 1), D_hom)
    muOfClust[2,1:2] ~ dmnorm(c(0.75, 1), D_hom)
    muOfClust[3,1:2] <- (muOfClust[1,1:2] + muOfClust[2,1:2])/2
    muOfClust[4,1:2] ~ dmnorm(c(0.5, 1),  D_err)
    mainClustAndOther[1:2] ~ ddirch(c(600, 30))

    tmp1 ~ ddirch(c(10, 10))
    pClust[1:2] <- mainClustAndOther[1] * tmp1[1:2]
    tmp2 ~ ddirch(c(2,2))
    pClust[3:4] <- mainClustAndOther[2] * tmp2[1:2]
}"

