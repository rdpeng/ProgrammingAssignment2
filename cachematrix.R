##The following two functions are used to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix creates Special object matrix and can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  inver <- NULL    
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setinverse <- function(solve) inver <<- solve
  # get the inverted matrix from cache
  getinverse <- function() inver
  # return the created functions to the working environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  inver <- x$getinverse()
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
if(!is.null(inver)) {
  message("getting cached data")
  # display matrix in console
  return(inver)
        ## Return a matrix that is the inverse of 'x'
}
  # create matrix since it does not exist in cache
  matrx <- x$get()
  inver <- solve(matrx, ...)
  # set inverted matrix in cache
  x$setinverse(inver)
  # display matrix in console
  inver
}


