## Caching the inverse of a matrix
## Caching the inverse of a matrix can increase the computational efficiency by reducing the complexity involved.
## The functions used below help to take a matrix as an input and caches its inverse
## The first function makeCacheMatrix creates a special matrix object that can cache its inverse




makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## cacheSolve computes the inverse of the result obtained from makeCacheMatrix
## If the inverse has been calculated already, then the result is obtained from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
      }

