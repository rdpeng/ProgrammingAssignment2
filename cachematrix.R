## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This makeCacheMatrix function caches the inverse of the matrix passed to it
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ ##setting the value of matrix    
     x <<- y
    inv <<- NULL
  }
  get <- function() x   ##getting value of matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function
## computes the inverse of an invertible square matrix if not computed
## if already computed then it gets from the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
  if(!is.null(inv)){ ##checks if the inverse has been calculated or not
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv  
}
