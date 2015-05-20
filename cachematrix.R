## This function creates a special "matrix" object 
## that can cache its inverse.
##
## m <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) )
## summary(m)
## m$get()
##      [,1] [,2]
## [1,]    1   12
## [2,]    2   13
## cacheSolve(m)
##            [,1]        [,2]
## [1,] -1.1818182  1.09090909
## [2,]  0.1818182 -0.09090909
## cacheSolve(m)
## getting cached data.
##            [,1]        [,2]
## [1,] -1.1818182  1.09090909
## [2,]  0.1818182 -0.09090909

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already 
## been calcullated, then the cachesolve should retrieve 
## the inverse from the cache.
##
## Given a makeCacheMatrix object, returns the inverse 
##   of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data.")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
}