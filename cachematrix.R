## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix object will set the matrix, get the matrix, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
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


## Write a short comment describing this function
# checks if it has already been computed inverse
# if it has not, get the matrix 
#find inverse, cache the result in object
#return result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
