### Introduction
In this function a special matrix is created, and an inverse is calculated
and returned.

The inverse is returned from teh cache if it is called again.

The following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

### to execute:
> source('~/cachematrix.R')

>a<-makeCacheMatrix( matrix(c(1:4), nrow = 2, ncol = 2) );

> cacheSolve(a)

Second time the cacheSolve(a) is called, the inverse is called
from the cache.
