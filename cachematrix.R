

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  




## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       m <- x$getInverse()
       if(!is.null(m)) {
             message("getting cached data")
             return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    
}


## Check
## m <- matrix(rnorm(9),3,3)
## steve  <- makeCacheMatrix(m)
## cacheSolve(steve)
##           [,1]       [,2]       [,3]
## [1,] -0.01152014 -0.6679522  0.1113437
## [2,]  0.33497936 -0.1816580  0.5231045
## [3,]  0.60035954  0.1571464 -0.4455123
##
## > cacheSolve(steve)
## getting cached data
## [,1]       [,2]       [,3]
## [1,] -0.01152014 -0.6679522  0.1113437
## [2,]  0.33497936 -0.1816580  0.5231045
## [3,]  0.60035954  0.1571464 -0.4455123

