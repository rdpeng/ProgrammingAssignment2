## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y) {
        x <<- y
        inversa <<- NULL
   }
    get <- function() x
    setinversa <- function(inversacalculada)(inversa <<- inversacalculada)
    getinversa <- function() inversa
    list(set = set, get = get, setinversa = setinversa,
       getinversa = getinversa)
}


cacheSolve <- function(x, ...) {
    inversa <- x$getinversa()
    if(!is.null(inversa)) {
         message("Datos obtenidos de cache")
         return(inversa)
    }
    
    dato <- x$get()
    inversa <- solve(dato, ...)
    x$setinversa(inversa)
    inversa
}
