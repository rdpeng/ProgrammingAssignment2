## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse
  # 4. get the inverse
  
  inverse = NULL
  set = function(y){
    x <<-y
  # '<<-' is used to assign a value to an object in an environment different from the current environment.
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inverse <<- inverse
  getinv<- function() inverse
  list(set = se, get = get,
       setinv=setinv,
       getinv=getinv)
  
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix bove.
cacheSolve <- function(x,...){
  ## return: inverse of the original matrix input to makeCacheMatrix()
   inverse=x$getinv()
  # if the inverse is already calculated
  if(!is.null(inverse)){
    message("getting cached data") # get it from cache and skip the calculation
    return(inverse)
  }
  # else compute the inverse
  matrixdata = x$get()
  inverse = solve(matrixdata,...)
  x$setinv(inverse)
  inverse
}
