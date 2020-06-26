## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   ##NB que esto significa que m es por 
##default una matrix vacia
        i <- NULL
        set <- function(y) {     ##creo la funcionalidad set en caso de que quiera modificar externamente el input x
                x <<- y
                i <<- NULL
        }
        
        get <- function () x    ##creo la funcionalidad get
        setinverse <- function(inverse) i <<- inverse   ##creo la funcionalidad setinverse
        getinverse <- function() i
        list(set = set, get = get,    ## le asigno nombres para poder llamarlas con el operador $
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        else {
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        
        ## Return a matrix that is the inverse of 'x'
} }
