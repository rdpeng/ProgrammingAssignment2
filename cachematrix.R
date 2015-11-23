## Put comments here that give an overall description of what your
## functions do
## Hello.  The following functions create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
##
## Sample run-time example included results
## > source("cachematrix.R")    load R program
## > a <- makeCacheMatrix()     create functions
## > a$set(matrix(1:4, 2, 2))   create matrix in working environment
## > cacheSolve(a)              1st run returns inverted matrix
##                              from working environment
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              2nd and subsequent runs
##                              returns inverted matrix from cache
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

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

        # get the value of the matrix
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
        ## attempt to get the inverse of the matrix stored in cache
        cache <- x$getInverse()

        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
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