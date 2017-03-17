# The following functions allow you to compute the inverse of an invertible
# matrix, and cache the result.

#' Creates a special "matrix" object that can cache its inverse
#'
#' @param x - a matrix, which is assumed to be invertible.
#'
#' @return
#' @export
#'
#' @examples
makeCacheMatrix <- function(x = matrix()) {
    inverseOfX <- NULL
    
    set <- function(y){
        x <<- y
        inverseOfX <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inverseOfX <<- inverse
    getInverse <- function() inverseOfX
    
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


#' Computes the inverse of the special "matrix" returned by makeCacheMatrix.
#' If the inverse has already been calculated (and the matrix has not
#' changed), then the cachesolve will retrieve the inverse from the cache.
#' 
#' @param x - the special "matrix" returned by makeCacheMatrix.
#' @param ...
#'   
#' @return - a matrix that is the inverse of 'x'
#' @export
#' 
#' @examples
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)){
        message("Using cached inverse")
        return(inverse)
    }
    
    xMatrix <- x$get()
    inverse <- solve(xMatrix, ...)
    x$setInverse(inverse)
    
    inverse
}
