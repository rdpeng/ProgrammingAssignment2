## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invA <- NULL
  set <- function(y){
    x <<- y
    invA <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
    invA <<- inverse
  getinverse <- function() invA
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invA <- x$getinverse()
  if(!is.null(invA)){
    message("getting cached data")
    return(invA)
  }
  mat <- x$get()
  invA <-solve(mat, ...)
  x$setinverse(invA)
  invA
}
