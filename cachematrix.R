## makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {

    # initialize the return value
    inverse <<- NULL

    # setter function to set a new value for the matrix
    # Superassignment of "mx" with new matrix
    set <- function(newmatrix) {
        mx <<- newmatrix
        inverse <<- NULL
    }

    # getter function that returns the value of "mx"
    get <- function() mx

    # setter function to assign the inverse matrix value
    # should not be called outside of the function cacheSolve()
    setinverse <- function(newvalue) inverse <<- newvalue

    #getter function to retrieve the inverse value
    getinverse <- function() inverse

    # return value of the makeCacheMatrix() is a list of functions
    # that can be invoked outside the function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the matrix
# returned by makeCacheMatrix above.
# If the inverse has already been calculated with no change to
# matrix, then cachesolve will retrieve the inverse
# from the cache.

cacheSolve <- function(mx, ...) {
        # retrieve inverse using the getter function
        inverse <- mx$getinverse()

        # if cache hit, return stored value
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }

        # else, generate inverse using solve() function
        data <- mx$get()
        inverse <- solve(data, ...)
        mx$setinverse(inverse)

        # return inverse matrix
        inverse
    }
