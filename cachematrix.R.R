## The first function aims to make a cache matrix object that can cache its inverse.

## The function aims to first create the matrix, set the values, compute the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  computeinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       computeinverse = computeinverse,
       getinverse = getinverse)
  
}


## The second function would return a matrix that is the inverse of 'x'
##It first checks to see if the inverse was already computed, and if it has, then it 
##gets that computed value. If not, it computes it and returns the output via computeinverse.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$computeinverse(s)
  s
}

