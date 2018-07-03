makeCacheMatrix <- function(x = matrix()) {

        ## Initializing the inverse.
                ini <- NULL

        ## Setting matrix.
                set <- function( matrix ) {
                mat <<- matrix
                ini <<- NULL
        }

         ## Getting the matrix.
                get <- function() {
    	 ## Return the matrix
    	        mat
        }

         ## Methodology to set the inverse of matrix.
                setInverse <- function(inverse) {
                inv <<- inverse
        }

         ## Methodology to get the inverse of matrix.
                getInverse <- function() {
         ## Return the inverse property
                inv
        }

         ## Returning of methodologies...
                list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
        }

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       }
}

## Computing the inverse of matrix returned via "CacheMatrixMaker" If the inverse has already been calculated then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

       ## Return a matrix that is the inverse of 'x'.
                 mat <- x$getInverse()

       ## Return inverse if its already computed.
                 if( !is.null(mat) ) {
                 message("getting cached data")
                 return(mat)
        }

        ## Get the matrix.
                 data <- x$get()

        ## Calculate inverse using matrix multiplication.
                 mat <- solve(data) %*% data

        ## Set the inverse.
                 x$setInverse(mat)

        ## Return the matrix.
                 mat
}
