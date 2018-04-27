## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) inversem <<- inverse
  getinverse <- function() inversem
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# This function computes the inverse of the special matrix returned by makeCacheMatrix and fetches the cached result if needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversem <- x$getinverse()
  if(!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
}
data <- x$get()
inversem <- solve(data)
x$setinverse(inversem)
inversem

}
  

  
  
}
