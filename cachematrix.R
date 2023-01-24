## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

CacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


CacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#TestCode. I used the first four prime numbers
B <- matrix(c(2,3,5,7),2,2)
B1 <- CacheMatrix(B)
CacheSolve(B1) #inverse returned after computation
