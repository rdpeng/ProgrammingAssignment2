#This Function would cache the inverse of a matrix which is usually 
#a time consuming process.

#The cache function would help by saving recomputing time and would  
#store all results of function calls in local Repository

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

#this function has created a special vector and stored it 
#in a matrix form and cached its inverse

cacheSolve <- function(x, ...) {
  p <- x$getinverse()
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}
