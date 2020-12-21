## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <-  NULO
  set <-  función ( y ) {
    x  << -  y
    inv  << -  NULO
  }
  obtener <-  función () { x }
  setInverse <-  función ( inversa ) { inv  << -  inversa }
  getInverse  <-  function () { inv }
  lista ( set  =  set , get  =  get , setInverse  =  setInverse , getInverse  =  getInverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'inv <-  x $ getInverse ()
  if ( ! is.null ( inv )) {
    mensaje ( " obteniendo datos encajados " )
    volver ( inv )
  }
  mat <-  x $ obtener ()
  inv <- resolver ( mat , ... )
  x $ setInverse ( inv )
  inv
}

