##This function returns the inverse of an inversible Matrix 

## This function holds the value of inverse matrix, and then
## defines the function to assign a new value of matrix
## in other environments. If new matrix, it takes it to the inverse (NULL). 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##The following function calculates the inverse of the matrix created with the above function.
##Retrieve the inverse from the cache if the inverse had already been calculated. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("Getting Cached Data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

