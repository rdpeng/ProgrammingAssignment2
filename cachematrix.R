## Week 3 assigment - Jalaja Uppili - Feb 23rd 2017
## Matrix Inversion is usually costly computation. It is therefore beneficial to 
## cache the inverse of a matrix.

## The following pair of functions cache the inverse of matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. For example,
## if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invm <<- inverse
  getinv <- function(inverse) invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)      

}

## Test

myMatrix <- makeCacheMatrix(matrix(1:4, 2,2))
myMatrix$get()
myMatrix$setinv()
myMatrix$getinv()


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  matr <- x$get()
  invm <- solve(matr, ...)
  x$setinv(invm)
  invm
}
          
cacheSolve(myMatrix)
cacheSolve(myMatrix)

## test

myMatrix$set(matrix(c(11,11,11,11), 2,2))
myMatrix$getinv()
myMatrix$setinv(222)
myMatrix$getinv()
