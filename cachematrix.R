#- Caching the inverse of a matrix in R. This is useful to avoid costly
#- repeated computation when dealing with a large and unchanging matrix.

#- Usage:
#-   source("cachematrix.R")
#-   sm <- makeSqrMatrix(500)      ## Create a 500x500 square matrix
#-   x <- makeCacheMatrix(sm)      ## Create the list of functions
#-   system.time(cacheSolve(x))    ## Compute/set/retrieve inv. matrix
#-   system.time(cacheSolve(x))    ## Retrieve from cache (fast)

#- Goal: Create custom functions to set the input matrix and its inverse
#-       matrix in the global environment and retrieve them.
#- Input: An numeric square matrix
#- Returns: A list of functions
makeCacheMatrix <- function(x = matrix()) {
    #- Local variable im
    im <- NULL

    #- Function to set the matrix x and a default NULL value as a
    #- placeholder for the inverse matrix in the global env
    #- (using the '<<-' operator)
    set <- function(y) {
            x <<- y
            im <<- NULL
    }
    #- Function to return the matrix
    get <- function() x
    #- Function to set the inverse matrix in the global environment
    setimat <- function(imat) im <<- imat
    #- Function to return the inverse matrix
    getimat <- function() im

    #-- Return a (named) list of functions
    list(set = set, get = get,
         setimat = setimat,
         getimat = getimat)
}

#- Goal: Using the custom set/get functions check if a NULL value is stored
#-       for the inverse matrix. If not, return inverse matrix or else compute
#-       / store / return a fresh one.
#- Input: List of set/get functions
#- Returns: inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #- Return inverse matrix if a non-null value is in the cache
        lm <- x$getimat()
        if(!is.null(lm)) {
                message("*** Getting cached data ***")
                return(lm)
        }

        #- Else, compute inverse matrix, store and return it.
        mdata <- x$get()
        lm <- solve(mdata, ...)
        x$setimat(lm)
        lm
}

#- Goal: Make a random k-dimensional square matrix
#- Input: A numeric dimension
#- Returns: A square matrix
makeSqrMatrix <- function(k) {
    set.seed(123)
    matrix(runif(k*k), k)
}
