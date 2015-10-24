## Esta función trata de explicar como funciona el lexican scoping en R
## This function try to explaint how lexical scoping works on R


## Esta función genera la Matrix a la cual se le hara una inversa
## This function make a Matrix which is use to make an inverse of that

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(z) {
                x <<- z
                inversa <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inversa <<- inverse
        getInverse <- function() inversa
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




}


## Esta función calcula la inversa de una matriz que se creo con la funcion
## makeCacheMatrix.
## This function computes the inverse of a given matrix created 
## with the function makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

}
