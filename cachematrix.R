## funcio
## functions do

## functions that cache the inverse of a matrix, 
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y) { #
        x <<- y # 
        inversa <<- NULL ##if null value is not applied in reverse
   }
    get <- function() x  #returns the value of the array that has
    setinversa <- function(inversacalculada)(inversa <<- inversacalculada)  #receives the value of the calculated inverse
    getinversa <- function() inversa  #observe the value of the calculated inverse
    list(set = set, get = get, setinversa = setinversa, 
       getinversa = getinversa)  #the list with the created functions is created
}


cacheSolve <- function(x, ...) {
    inversa <- x$getinversa()    #look for the data
    if(!is.null(inversa)) {  #if it already exists it is obtained from cache, if it is not searched
         message("Datos obtenidos de cache")
         return(inversa)  #return inverse of X
    }
    
    dato <- x$get()  # assign the value of list X in get to dato
    inversa <- solve(dato, ...)  #calculate inverse
    x$setinversa(inversa) 
    inversa  #return inverse of X
}
