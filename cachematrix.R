## Put comments here that give an overall description of what your
## functions do

## This function store the matrix for cacheing the value of the inverse. The function does not calculate the inverse but only set the 
## value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL  ## initialize the value of the inverse as null
 setmatrix <- function (y= matrix(y){
   x<<-y ## this is used to overwrite the value of the old matrix with new in the global variable x
   inverse<<- NULL ## this flush out the old inverse value from the global varible
 }
 ## these functions are used by the cachesolve function to get, set the value of x and its inverse in global variable
 getmatrix<- function() x
 setinverse <- function (inverse) inverse <<- inverse
 getinverse <- function() inverse
 list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse )
}

# This function does the actual inverse of matrix through the solve function, it uses the above defined functions to store the value

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse() 
  if(!is.null(inverse)) { ## if function checks if the inverse value is avilable in the global variable, if avilable it is printed otherwise calculated in the else section
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
