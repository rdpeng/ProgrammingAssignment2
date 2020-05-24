##  This function consists of two parts, which cache the inverse of matrix.
## In the first part a special matrix is created.



makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The second part of this function calculates the inverse of the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
          }
        data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

