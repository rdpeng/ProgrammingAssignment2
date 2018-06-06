## Put comments here that give an overall description of what your functions do

## makeCacheMatrix function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  inverse <- NULL                             
  set <- function(y) {                     
    x <<- y                            
    inverse <<- NULL                        
  }
  get <- function() x                     
  setinverse <- function(inverse) inverse <<- inverse  
  getinverse <- function() inverse                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## The function cacheSolve computes the inverse of the matrix above
## If the inverse was already calculated and the matrix has not changed, it retrieves it from the cache

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## Test - X should be the same as X4, since we are inversing twice
x <- matrix(rnorm(9),3,3)
x1 <- makeCacheMatrix(x)
x2 <- cacheSolve(x1)
x3 <- makeCacheMatrix(x2)
x4 <- cacheSolve(x3)
