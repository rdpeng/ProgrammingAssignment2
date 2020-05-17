## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matx <- x$get()
  inverse <- solve(matx)
  x$setInverse(inverse)
  inverse  
}

##TestCase1

##TestMatrix <- matrix(1:4,2,2)
##TestMatrix
##           [,1] [,2]
##      [1,]   1    3
##      [2,]   2    4

##CacheMatrix <- makeCacheMatrix(TestMatrix)
##CacheMatrix$get()
##              [,1] [,2]
##        [1,]    1    3
##        [2,]    2    4

##CacheMatrix$getInverse()
##NULL

##cacheSolve(CacheMatrix)
##            [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5

##cacheSolve(CacheMatrix)
##   getting cached data
##           [,1] [,2]
##      [1,]  -2  1.5
##      [2,]   1 -0.5
