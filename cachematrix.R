## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache


makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        # get the value of the matrix using get function
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache
        
        # return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)

}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache


cacheSolve <- function(x, ...) {
       
        cache <- x$getInverse()
        
       # return inverted matrix from cache if it exists else create the matrix in working environment
        if (!is.null(cache)) {
                message("getting cached data")
                
                # display matrix in console
                return(cache)
        }
        
        # create matrix since it does not exist
        matrix <- x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(cache)
        } )
        
        # display matrix in console
        return (cache)
}
