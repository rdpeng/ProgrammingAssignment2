## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL

  ## Define unafunción para asignar a "x" el valor proporcionado como "y".
  set = function(y) {
    x <<- y
    inv <<- NULL ## Asigna nulo a la variable de la función invocante.
  }
  
  get = function() x
  
  setinv = function(inverse) inv <<- inverse 
  
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("obteniendo datos en cache...")
    return(inv)
  }
  
  mat.data = x$get()
  
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}