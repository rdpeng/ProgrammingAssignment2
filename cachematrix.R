## Producing the inverse of matrices is a quite heavy computation, as well as time consuming. Therefore, the 
## functions in this script makes it possible to store the inverse of an invertible matrix in a list and,
## if the inverse has been computed earlier, fetch it from the cache. 

## makeCacheMatrix will take an invertible matrix and populate a list holding the variables needed to store
## the original matrix and its inverse. 
## In short, this function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <-  function(X = matrix()) {

  invX <- NULL
  set <- function(Y) {
    X <<- Y
    invX <<- NULL
  }
  get <- function() X
  setInv <- function(solve) invX <<- solve
  getInv <- function() invX
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(X,...) {

  invX <- X$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- X$get()
  invX <- solve(data, ...)
  X$setInv(invX)
  invX
}

## TESTING ## 
#Initiate invertible matrix
A <- matrix(c(5, 1, 0,
              3,-1, 2,
              4, 0,-1), 
          nrow=3, byrow=TRUE)

#Populate a new variable holding both original matrix and an inverted version
invTest <- makeCacheMatrix(A)
# Print original matrix
invTest$get()

#First run, no cached data available since invX = NULL
cacheSolve(invTest)
#Second run of cahcesolve(), invX populated with the inverse and therefore we grab the cached result. invX <> NULL
cacheSolve(invTest)
