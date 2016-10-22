## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix returns a matrix which will be used as an input to cacheSolve function for the inversion of matrix

makeCacheMatrix <- function(x = matrix()) {
 ## x is defined as a square invertible matrix
 ## Function makeCacheMatrix returns a matrix which will be used as an input to 
 ##  used as the input to cacheSolve()
invertM <- NULL 
 set <- function(x) {
 x <<- y
 invertM <<- NULL # variable defined to store inversion
 }
 get <- function() x
 setinvertM <- function(inverse) invertM <<- inverse
 getinvertM <- function () invertM
 list (set=set, get=get, setinvertM=setinvertM, getinvertM=getinvertM)
}


## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 invertM <- x$getinvertM()
   if (!is.null(invertM))
  {
  message("getting cahced data")
  return(invertM)
  }
  data = x$get()
  invertM = solve(data, ...)
  x$setinvertM(invertM)
  invertM
}
