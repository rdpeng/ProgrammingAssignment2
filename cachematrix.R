## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This fucntion let to calculate the inversed matrix of the initial matrix
##First of all, we set the value inv to null, in theses variable, we will set the inversed matrix
##Then, we set the initial matrix,so we get it again
##We calculate the inversed matrix with solve function
#get the inverted matrix and put it all in a list that enables to send it as an argument
##for the cacheSolve fucntion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
##In this funciton we check if the fucntion has been called before with
# the if condition, and if not, we calcule the inverse with solve function
#set it on the result variable and return it


cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()#get the value of the input matrix
  inv = solve(mat, ...)
  
  x$setinv(inv)
  
  return(inv)
}
