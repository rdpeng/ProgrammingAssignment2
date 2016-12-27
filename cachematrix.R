## Put comments here that give an overall description of what your
## functions do

  ## Las dos funciones siguientes mantienen en memoria cache y calculan 
  ## la inversa de una matriz.
  ## Write a short comment describing this function
  
  ## Esta funcion crea un objeto especial de nombre "matrix" que puede situar en la 
  ## memoria cache su inversa.
  makeCacheMatrix <- function(x = matrix()) {
    
      inverse <- NULL
      set <- function(x) {
           mtx <<- x;
           inverse <<- NULL;
         }
       get <- function() return(mtx);
       setinv <- function(inv) inverse <<- inv;
       getinv <- function() return(inverse);
       return(list(set = set, get = get, setinv = setinv, getinv = getinv))
  }
  
  
  ## Write a short comment describing this function
  
    ## Esta funcion calcula la inversa del objeto especial  "matrix"  que retorna 
    ## la funcion anterior `makeCacheMatrix` . En el caso que ya se haya 
    ## calculado (y la matriz no haya cambiado), entonces
    ## `cacheSolve` deberia recuperar la inversa desde la memoria cache.
    cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
       inverse <- mtx$getinv()
       if(!is.null(inverse)) {
           message("Obteniendo datos desde la memoria cache...")
           return(inverse)
         }
       data <- mtx$get()
       invserse <- solve(data, ...)
       mtx$setinv(inverse)
       return(inverse)
    }

    
