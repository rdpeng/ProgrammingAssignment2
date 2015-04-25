## Put comments here that give an overall description of what your
## functions do
##There are 2 functions,, which is used to set the Martix and its Inverse Marrix in cache

## Write a short comment describing this function
##it takes a matrix as a x argument. Using set function it set into cache and makes it inverse to null.
## defines get functions to retrive it back.
## setinverse function is to compute inverse matrix using solve function and stores it in cache
## getinverse function is to retrive the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## cacheSolve functions is to get the inverse of matrix if already set , if not i computes and set it in caches and sends it back.
cacheSolve <- function(x, ...) {
   m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
