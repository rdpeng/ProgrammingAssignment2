## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL

  ## Define una función para asignar a "x" el valor proporcionado como "y".
  set = function(y) {
    x <<- y
    inv <<- NULL ## Asigna nulo a la variable de la función invocante.
  }
  
  ## Define función para obtener valor de x
  get = function() x
  
  ## Asigna el valor inverso
  setinv = function(inverse) inv <<- inverse 
  
  ## Obtiene el valor inverso
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  ## Si ni es nulo el valor inverso, lo devuelve y termina
  if (!is.null(inv)){
    message("obteniendo datos en cache...")
    return(inv)
  }
  
  ## Obtiene el valor 
  mat.data = x$get()
  
  ## Determina elvalor inverso
  inv = solve(mat.data, ...)
  
  ## Establece elvalor inverso
  x$setinv(inv)
  
  return(inv)
}