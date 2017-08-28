exampleModelParameters <- list(D_hom = diag(2)*4, V_hom = cbind(c(0.005, 0), c(0, 0.1))/3, n_hom = 30, D_err = diag(2), V_err = diag(2)*10/3, n_err = 300, V_het = diag(2)*0.025/3, n_het = 1500)
exampleDbscanParameters <- list("1" = list(eps = 0.04, minPts = 65), "2" = list(eps = 0.03, minPts = 105), "3" = list(eps = 0.06, minPts = 65), "4" = list(eps = 0.02, minPts = 45))

