## Two functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## Intialize 2 objects: 'x', 'm'
    ## 'x' is initialized as a function argument
    ## 'm' is set to NULL

    m <- NULL

    ## Create setter and getter methods:

    ## Matrix set method
    set <- function(y) {
            ## Assign y-value to 'x' object in parent environment
            ## Assign NULL-value to 'm' object in parent environment
            x <<- y
            m <<- NULL
     }

    ## Matrix get method
    get <- function() x

    ## Matrix inverse set method
    ## Assign the input argument to the value of 'm' in the parent environment
    setinverse <- function(solve) m <<- solve

    ## Matrix inverse get method
    getinverse <- function() m

    ## Return the getters and setters as elements of named-list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then the cachesolve should retrieve the
##  inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Get inverse matrix of 'x'
    m <- x$getinverse()

    ## Check if inverse was cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## Get matrix
    data <- x$get()
    m <- solve(data, ...)

    ## Set matrix
    x$setinverse(m)

    ## Return matrix
    m

}

## Example for testing:
## a = matrix(c(2,0,2,1,2,0,0,0,1),3, 3)
## b <- makeCacheMatrix(a)
## cacheSolve(b)
## cacheSolve(b)
