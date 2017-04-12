## The computations of matrix inversion are often time-consuming. By using this function instead of
## the solve() function, the computation results of matrix inversion can be cached to avoid 
## repeating the same time-consuming computiation. This function can check if the computations
## have been done (the result have been cached) and then can chose to compute the inverse of matrix
## or extract the cached result which have been computed before.

## 'makeCacheMatrix' can create a list of funtion to 
## 1.set the value of the matrix 
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invrs <<- solve
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' will return the inverse of the matrix. First it will check if the computation has
## been done (the result have been cached). If yes, the function will return the result computed 
## before from the cache and skip the computation; if not, it will compute the inverse of the 
## matrix, cache the result (set the value of inverse) by 'setinverse' function and return the 
## result.

cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs ## Return a matrix that is the inverse of 'x'
}
