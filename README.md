### Introduction

Repository for Peer Review Programming Assignment 2 of the R Programming Course by Bloomberg School of Public Health, Johns Hopkins University.

### Example Usage

#### Basic Usage

> m <- matrix(rpois(25, lambda=4),5)  # Creates a 5*5 matrix of random numbers from a Poisson Distribution 
> cache <- makeCacheMatrix(m)         # Creates the Cache list
> cache$get()                         # Return the original matrix
> cacheSolve(cache)                   # Computes the matrix inverse
> cacheSolve(cache)                   # Again computed the matrix inverse, but its fetched from cache,
						 will see output like "getting cached matrix inverse"

#### Performance impact due to Caching:

> x <- 1000
> m <- matrix(rpois(x^2, lambda=4), x)
> cache <- makeCacheMatrix(m)
> t <- system.time(cacheSolve(cache))
> t
   user  system elapsed 
  2.663   0.018   2.683 
> t <- system.time(cacheSolve(cache))
getting cached matrix inverse
> t
   user  system elapsed 
      0       0       0 
