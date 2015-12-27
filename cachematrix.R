## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
 setmatrix <- function (y= matrix(y){
   x<<-y
   inverse<<- NULL
 }
 getmatrix<- function() x
 setinverse <- function (inverse) inverse <<- inverse
 getinverse <- function() inverse
 list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse )
}

# Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)) {
    message("getting cache inverse value")
    return(inverse)
  } else {
    matrix<-x$getmatrix()
    inverse<-solve(matrix)
    x$setinverse(inverse)
    inverse
  }
        ## Return a matrix that is the inverse of 'x'
}
