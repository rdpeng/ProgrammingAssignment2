##The functions "makeCacheMatrix" and "cacheSolve" 
##together cache the inverse resolution of a square matrix.

##"makeCacheMatrix" is a function that creates in the 
## cache a space to save the inverse of a square matrix.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(x){
    m <<- x
    i <<- NULL
  }
  get <- function()m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##"cacheSolve" is a function that resolves 
##the inverse of a square matrix and this is saved in cache

cacheSolve <- function(m, ...) {
  i <- m$getInverse()
  if(!is.null(j)){
    message("Data in Cache")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  m$setInverse(i)
  i
}


