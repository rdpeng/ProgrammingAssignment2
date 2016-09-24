## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    #Set function and assign y to x
  }
  get <- function() x
  setminverse <- function(minverse) m <<- minverse
  getminverse <- function() minverse
  list(set = set, get = get,
       setminverse = setminverse,
       getminverse = getminverse)
       #get inverse
}

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {m <- x$getminverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setminverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
