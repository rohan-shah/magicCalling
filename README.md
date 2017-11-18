# Introduction

Realising the potential of large genetic resources requires the ability to perform genotyping efficiently. In populations with tens or hundreds of thousands of SNP markers, it is infeasible to assign genotypes manually; this must be done automatically. This problem is generally solved by the application of mixture models (Xiao et al., 2007, Teo et al., 2007). 

Our focus is on large populations generated according to some experimental design; e.g. the Collaborative Cross  (Threadgill and
Churchill, 2012) or MAGIC (Huang et al., 2015). These populations are substantially inbred, with a small amount of residual heterozygosity. If genetic lines can be identified which are heterozygous at a QTL locus, these lines can be used for fine-mapping of that QTL. 

This package implements model-based clustering method for identifying marker heterozygotes in significantly inbred populations. We focus on data generated by the Infinium iSelect platform. 

# Installing this package

This package relies on v5 of the JAGS (Just Another Gibbs Sampler) package (Plummer, 2015) to fit models, through its R frontend R/rjags. These packages are somewhat challenging to build. 

We provide a suitable windows binary of the rjags package under the releases section, compiled using Visual Studio 2015. If this fails to install, you may need to install the Visual C++ redistributable package. 

For Linux you will need to download and install both JAGS and R/rjags from source, from [sourceforge](https://sourceforge.net/projects/mcmc-jags/). Note that, as of the time of writing, version 5 is only available from verison control, and not as a packaged download. 

# Using this package

The main function provided by this package is 

> fitClusterModel(data, startingPoints, n.iter, D_hom, V_hom, n_hom, D_err, V_err, n_err, V_het, n_het)

Due to typesetting restrictions, we will refer to the more mathematical parameters as 

> **D**(hom), **V**(hom), n(hom), **D**(err), **V**(err), **n**(err), **V**(het), n(het))

We explain these parameters here, but for a better typeset explanation, see the application note accompanying the release. 

This function fits a four-component normal mixture model, with a hierarchical prior. The first two components correspond to the homozygous marker alleles, and have bivariate normal distributions N(µ1, Σ1) and N(µ2, Σ2). The third component represents the marker heterozygotes, and has distribution N(0.5*(µ1+µ2), Σ3), so the mean is the average of the means for the homozygote components. The fourth component represents errors, and has distribution N(µ4, Σ4). 

The priors for the mean parameters µ1, µ2 and µ4 and covariance matrices Σ1, ..., Σ4 are defined using multivariate normal and Wishart distributions. The prior for the mixing proportions **p** = (p1, ..., p4) is defined using Dirichlet distributions. Further details about the hierarchical priors are given at the end of this document.

An example of using this model is:
```R
data("eightWayExampleData", package="magicCalling")
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
result <- fitClusterModel(data, startingPoints, n.iter = 200, D_hom = diag(2)*4, V_hom = cbind(c(0.005, 0), c(0, 0.1))/3, n_hom = 30, D_err = diag(2), V_err = diag(2)*10/3, n_err = 300, V_het = diag(2)*0.025/3, n_het = 1500)
plot(result, chainIndex = 1)
```
The resulting plot is
![Fitted model](/data/example.png)

# Appendix

For a properly typeset version of this section, see the application note accompanying the release. 

Let **p** = (p1, p2, p3, p4) be the mixing proportions. Our prior knowledge is that p1 + p2 is much larger than p3 + p4 , because there will be many
more marker homozygotes than heterozygotes and errors. However one of p1 or p2 may be small, as the marker allele distribution is unknown. To account for this information, we define our prior as follows. Let p(hom) be the proportion of homozygotes, and let (p(hom) ,1 − p(hom)) have a Dirichlet(600,30) distribution. Let p(a, hom) be the proportion of homozygotes that fall into the first cluster, and let (p(a, hom), 1 − p(a, hom)) have a Dirichlet(10, 10) distribution.

Let p(het) be the proportion of heterozygotes among all lines that are either heterozygotes or errors, and let (p(het), p(err)) have a Dirichlet(2,2) distribution. The mixing proportions are 
> **p** = (p(hom) p(a, hom), p(hom) (1 − p(a, hom)),(1 − p(hom)) p(het), (1 − p(hom))p(err)).

The priors for the mean parameters are
> µ1 ~ N((0.25,1), **D**(hom), 

> µ2 ~ N((0.75, 1), **D**(hom)),

> µ4 ~ N((0.5,1), **D**(err)),

where µ1, µ2 and µ4 are independent and **D**(hom) and **D**(err) are
deterministic covariance matrices. The final prior that must be specified is
for Σ1, ..., Σ4 . We use the Wishart distribution, parameterised as
> Σ1, Σ2 ~ Wishart(n(hom) **V**(hom), n(hom)),

> Σ3 ~ Wishart(n(het) **V**(het), n(het)),

> Σ4 ~ Wishart(n(err) **V**(err), n(err)).

The matrix parameters are the expected values of the covariance matrices.
The real parameters n represent the degree to which the covariance matrix
distribution is concentrated around its expectation; a large value indicates
a distribution highly concentrated around the matrix parameter. A small
value indicates a flatter distribution.