## Programming Assignment # 2
###############################################################################################
#
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
###############################################################################################

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  init <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  fetch <- function() x
  initinverse <- function(inverse) invrs <<- inverse
  fetchinverse <- function() invrs
  list(init=init, fetch=fetch, initinverse=initinverse, fetchinverse=fetchinverse)
}

##############################################################################################
#
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.
#
##############################################################################################
cacheSolve <- function(x, ...) {
  invrs <- x$fetchinverse()
  if(!is.null(invrs)) {
    message("Fetching cached data ...")
    return(invrs)
  }
  data <- x$fetch()
  invrs <- solve(data)
  x$initinverse(invrs)
  invrs
}
################################################################################################
#Test Run / Vectors
################################################################################################
#> x = rbind(c(2, -1/2), c(-1/8, 8))
#> mcm = makeCacheMatrix(x)
#> mcm$fetch()
#[,1] [,2]
#[1,]  2.000 -0.5
#[2,] -0.125  8.0
#> cacheSolve(mcm)
#[,1]       [,2]
#[1,] 0.501960784 0.03137255
#[2,] 0.007843137 0.12549020
#> cacheSolve(mcm)
#Fetching cached data ...
#[,1]       [,2]
#[1,] 0.501960784 0.03137255
#[2,] 0.007843137 0.12549020
