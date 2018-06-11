## Objective is to write two functions makeCacheMatrix and cacheSolve
## that cache the inverse of a matrix

## makeCacheMatrix creates matrix object that can cache its inverse
## for the input

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing and set
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get<- function(){
    ## Return matrix
    x
  }
  
  ## Set inverse of matrix
  setInv <- function(inverse_matrix){
    inv <<- inverse_matrix
  }
  
  ## Get inverse of matrix
  getInv <- function(){
    ## Return inverse
    inv
  }
  
  ## list of methods
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


  

## CacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## above.If already computed then cacheSolve will retrieve from cache

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if( !is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## Compute the inverse via matrix multiplication
  m <- x$get()
  inv <- solve(m, ...)
  
  ## Set the inverse to object
  x$setInv(inv)

  ## Return it
  inv
}
