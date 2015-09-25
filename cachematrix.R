# This Function creates a special object that stores a matrix
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              invs <- NULL
              set <- function(b) {
                x <<- b
                invs <<- NULL
              }
              get <- function() x
              setsolve <- function(solve) invs <<- solve
              getsolve <- function() invs
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        invs <- x$getsolve()
        if(!is.null(invs)) {
          message("getting cached data")
          return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setsolve(invs)
        invs
}

## Working Example and results
## > source(cachematrix.R)
## > x <- rbind(c(2, 3.5), c(3.5, 2))
## > k <- makeCacheMatrix(x)
## > k$get()
##      [,1] [,2]
## [1,]  2.0  3.5
## [2,]  3.5  2.0
## > cacheSolve(k)
##            [,1]       [,2]
## [1,] -0.2424242  0.4242424
## [2,]  0.4242424 -0.2424242
## > ## After creating the cache, the second run shows
## > cacheSolve(k)
## getting cached data
##            [,1]       [,2]
## [1,] -0.2424242  0.4242424
## [2,]  0.4242424 -0.2424242
## >
## >
