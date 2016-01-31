## The first of these two functions create a special matrix and that stores a matrix and cache's its inverse. 
## The second function calcuates the inverse of a passed matrix but before calculating the inverse it checks to see
## if the inverse to that same matrix exists in cache.

## makeCacheMatrix creates a special matrix, which is really a list containing a function to 1. set the value of the matrix
## 2. get the value of the matrix 3. set the value of the inverse 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the special matrix already created but first it checks to see if the inverse already
## exists and that the matrix is the same. If the inverse already exists the new matrix is the same as the cached inverse
## then the inverse is returned from cache and not calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        ## Check if the value has been already calculated
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## Solves and stores the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
