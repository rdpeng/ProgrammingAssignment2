## Put comments here that give an overall description of what your
## functions do
## These functions implement caching for matrix inversion

## Write a short comment describing this function
## 'makeCacheMatrix' creates a list of functions that will set/get
## an input value (matrix) and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL        ## initializes i as NULL. i will be used to cache the inverse of the matrix x
  set <- function(y) { 
    x <<- y        ## stores the matrix passed to it in parent x, the cached matrix
    i <<- NULL     ## clears i
  }
  get <- function() x ## returns the cached matrix
  setinverse <- function(inverse) i <<- inverse ## stores passed inverse in i
  getinverse <- function() i    ## returns cached inverse i
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse) ## exposes a list of the subfunctions allowing them, to be called
}


## Write a short comment describing this function
## 'cacheSolve' takes a matrix created by 'makeCacheMatrix' and uses it
## to retrieve the cached inverse, or calculate and cache an inverse 
## if there isn't one already in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## get the inverse from the cache. it will be Null if not cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } ## if the cached inverse was not null, return it
  data <- x$get() ## if the cached inverse was null, we continue here. get the matrix from x as data
  i <- solve(data, ...) ## solve for the inverse of data
  x$setinverse(i) ## cache the result
  i ## return the inverse
}
