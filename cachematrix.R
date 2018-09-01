## makeCacheMatrix and cacheSolve are functions that create a special "matrix" object
## whose inverse can be solved and cached to save computational resources

## makeCacheMatrix allows for the creation of a special "matrix" class of object whose values can be initialized,
## set, and reset. The function returns a list, which allows the usage of the '$' extractor to access the
## functions within the parent function.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { 
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve is a function that works hand-in-hand with the makeCacheMatrix function.
## It computes the inverse of the defined matrix in a makeCacheMatrix object and returns the solution.
## If a solution is saved in memory, it returns the cached data instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("Getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

