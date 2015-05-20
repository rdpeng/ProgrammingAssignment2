## This function creates a special "matrix" object 
## that can cache its inverse.
##
## m <- makeCacheMatrix( matrix(c(1,2,0,9,0,8,0,5,6), nrow = 3, ncol = 3) )
## summary(m)
## m$get()
##      [,1] [,2] [,3]
## [1,]    1    9    0
## [2,]    2    0    5
## [3,]    0    8    6
## cacheSolve(m)
##             [,1]        [,2]        [,3]
## [1,]  0.27027027  0.36486486 -0.30405405
## [2,]  0.08108108 -0.04054054  0.03378378
## [3,] -0.10810811  0.05405405  0.12162162
## cacheSolve(m)
## getting cached data.
##             [,1]        [,2]        [,3]
## [1,]  0.27027027  0.36486486 -0.30405405
## [2,]  0.08108108 -0.04054054  0.03378378
## [3,] -0.10810811  0.05405405  0.12162162
##
##
## a <- makeCacheMatrix( matrix(c(9,8,1,2), nrow = 2, ncol = 2) )
## a$get()
##      [,1] [,2]
## [1,]    9    1
## [2,]    8    2
## cacheSolve(a)
##      [,1] [,2]
## [1,]  0.2 -0.1
## [2,] -0.8  0.9
## cacheSolve(a)
## getting cached data.
##      [,1] [,2]
## [1,]  0.2 -0.1
## [2,] -0.8  0.9
##

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix
  invs <- NULL
  ## to set the matrix
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  ## to get the matrix
  get <- function() x
  ## to set the inverse of the matrix
  setinverse <- function(inverse) invs <<- inverse
  ## to get the inverse of the matrix
  getinverse <- function() invs
  ## Return the list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already 
## been calcullated, then the cachesolve should retrieve 
## the inverse from the cache.
##
## Given a makeCacheMatrix object, returns the inverse 
##   of the matrix
##
## Computing the inverse of a square matrix can be done with the "solve"
## function in R. For example, if "data" is a square invertible matrix, then
## solve(data)" returns its inverse.

cacheSolve <- function(x, ...) {
  
  ## to get the inverse of the x matrix
  invs <- x$getinverse()
  ## if its already set, get the inverse matrix from the cache
  if(!is.null(invs)) {
    message("getting cached data.")
    return(invs)
  }
  ## Get the matrix from x
  data <- x$get()
  ## computing the inverse using solve function
  invs <- solve(data)
  ## to set the inverse matrix
  x$setinverse(invs)
  ## Return the inverse matrix
  invs
}