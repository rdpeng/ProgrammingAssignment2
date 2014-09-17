##' This function creates a special "matrix" object 
##' that can cache its inverse
#' @param x An invertable matrix.
#' 
#' @return The inverse of the matrix.
#' @examples
#' makeCacheMatrix(c(2,4,6,8),2,2)
#' 



makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                print("we are in the set function now")
                x <<- y
                i<<- NULL
        }
        get <- function() {
                print ("we are in the GET function now")
                x
        }
        setinverse <- function(inverse) {
                print ("we are in the SETINVERSE function now")
        
                i <<- inverse
        }
        getinverse <- function(){ 
                print("we are in the GETINVERSE function now")
                i
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##' This function computes the inverse of the special "matrix" 
##' returned by makeCacheMatrix function. If the inverse 
##' has already been calculated (and the matrix has not changed), 
##' then the cachesolve should retrieve the inverse from the cache.
##' 
#' @param x An invertable matrix.
#' 
#' @return The inverse of the matrix.
#' @examples
#' cacheSolve(c(2,4,6,8),2,2)
#'


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
