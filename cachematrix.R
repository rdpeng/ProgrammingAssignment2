## Put comments here that give an overall description of what your
## functions do

#La inversión de matrices suele ser un cálculo costoso y puede ser beneficioso almacenar en caché la inversa de una matriz en lugar de calcularla repetidamente. 
#Por eso es que usamos estas funciones para almacenar en caché la inversa de una matriz.

## Write a short comment describing this function
#Esta función crea un objeto matricial especial que puede almacenar 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#Esta función calcula la inversa de la "matriz" especial devuelta por la función anterior. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
        if(!is.null(inv)) {
                message("dar resultado caché")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
#Si ya se ha calculado la inversa (y la matriz no ha cambiado), 
        #entonces la solución de caché debería dar la inversa de la caché.

}
