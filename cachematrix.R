## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  Get and Set Function list and inverse ay matrix


makeCacheMatrix <- function(x = matrix()) {
    ## Inicializar las variables de inverso
    i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    get <- function() {
        m
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## this function have 3 parts, gets the inverse matrix
## get de matrix in cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
        
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
