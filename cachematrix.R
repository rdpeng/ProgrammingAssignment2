## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( m = matrix() ) {

	## Se inicia la Inversa
    inv <- NULL

    ## Configura la matriz
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ## Obtine la matriz
    get <- function() {
    	## Se entrega la matriz
    	m
    }

    ## Metodo para calcular la inversa
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Se obtinene la inversa
    getInverse <- function() {
        ## Se entrega la inversa
        inv
    }

    ## Se entrega una lista de los metodos
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calcula la inversa de la matriz especial entregada por la función "makeCacheMatrix"
## calculada arriba. En caso que la matriz inversa ya estuviesse calculada, la siguiente finción,
## "cachesolve", deve recuperar la matriz inversa calculada del cache.
cacheSolve <- function(x, ...) {

    ## Se entrega una matriz que representa la inversa de "x"
    m <- x$getInverse()

    ## Se entrega la inversa, en caso que esta se corresponda
    if( !is.null(m) ) {
            message("Obteniendo datos del caché")
            return(m)
    }

    ## Se busca la matriz en el objeto calulado
    data <- x$get()

    ## Calcula la inversa
    m <- solve(data) %*% data

    ## Ajusta la inversa al objeto
    x$setInverse(m)

    ## Se entrega la inversa de la matriz "m"
    m
}
