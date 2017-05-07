##Jennifer Ogle Assignment 2 R Programming

## These functions cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  #function takes matrix input

        inverseMatrix <- NULL #inverse matrix initialized
        
        set <- function (y){  #set Matrix value
                x <<- y
                inverseMatrix<<-NULL
        }
        get <- function() x # get Matrix Value
        setInverse <- function(inverse) inverseMatrix <<- inverse  # set inverse Matrix Value
        getInverse <- function() inverseMatrix # get inverse Matrix value
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
}


## This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
        
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("Getting Cached Invertible Matrix")   #if not NULL, show message
                return(inverseMatrix)
        }
        data <- x$getMatrix()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        return(inverseMatrix)
}
