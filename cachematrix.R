## These functions inverts matrices in an easy way
## This is my second programming assigment in the R Programming course
## Maria Bran.

## makeCacheMatrix takes the base of the makeVector example
## cache the inverse of a matrix
## first we eliminate any error if x is not a matrix
## then we create the function to get the matrix
## finally there's the function that inverts and gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inve <<- inverse
  getinverse <- function() inve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## Compute and cache the inverse of a matrix
## this function is also based on the example taken from the course
## "This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from 
## the cache." (taken from the assigment)

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)){
    message("getting inversed matrix")
    return(inver)
  }
  data <- inver$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
