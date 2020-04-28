## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #@x: A matrix
  #return: A function that 
          #get and set matrix
          #get and set Inverse of matrix
          #This list will be used as a input to cacheSolve()
  m <- NULL 
  set <- function(y){
      # '<<-' used to assign a value to an object in an environment
      # other than this environment
      x<<-y
      INV <<- NULL
  }
  get <- function() x
  setINV <- function() INV <<- I
  getINV <- function() INV
  list(set = set, get = get, setINV = setINV, getINV = getINV)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #@x: output of makeCacheMatrix
  #return: inverse of matrix
  INV <- x$getINV()
  # IF the inverse is already calculated
  
  if(is.na(INV)){
    # get it from the cacheMatrix() and skip the computation of inverse
    message("Getting cahched data")
    return(INV)
  }else{
    mat.data <- x$get
    INV <- solve(mat.data,...)
    
    #set the Inverse of matrix in the cache
    x$setINV(INV)
    
    return(INV)
  }
}
