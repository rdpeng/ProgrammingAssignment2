## makeCacheMatrix esta función crea una "matriz" especial que puede almacenar en caché su inversa

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() 
	x
        setinverse <- function(inv)
	m <<- inv
        getinverse <- function()
	m
        list(setMatrix = setMatrix, 
	     getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve es una función que calcula la inversa de la "matriz" especial devuelta por la función makeCacheMatrix. 
## la función cacheSolve debe recuperar la matriz calculada, si la matrix no ha cambiado. Se debe dar la matriz
## la inversa de la memoria caché

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Retorna una matriz que es la inversa de 'x'
}
