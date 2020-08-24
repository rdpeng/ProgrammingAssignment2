  ## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##We define our as null the variable that's going to store de inverted matrix
  inv <- NULL
  ## We create the method to define the variable that's going to contain the normal matrix
  set <- function(y){
    ##We define the values
    x<<- y
    inv <<- NULL
  }
  
  ## We create the objects that give us acces to the inverted matrix
  get <- function(){x}
  SetInverse <- function(inverse){
    inv <<- inverse
  }
  
  getInverse <- function(){
    inv
  }
  
  list(set = set, get = get, SetInverse = SetInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## We define the variable inv using the method getInverse
  inv <- x$getInverse()
  ##We identify if we have already inverted the matrix
  if(!is.null(inv)){
    ##We just print the already created inverted matrix
    message("get cached data ")
    return(inv)
  }
  
  ## In case the value of inv it's null we invert the variable and then we assign the inverted matrix to inv using the method setInverse and then we print the result.
  mat <- x$get()
  inv <- solve(mat,...)
  x$SetInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
