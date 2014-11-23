## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # inv_cache will store the cached inverse matrix
    inv_cache <- NULL
    # Setting parameters for the matrix
    set <- function(y) {
        x <<- y
        inv_cache <<- NULL
    }
    # to get the matrix
    get <- function() x
    # Setter for the inverse
    setinv <- function(inverse) inv_cache <<- inverse
    # Getter for the inverse
    getinv <- function() inv_cache
    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)
    # Cache the inverse
    x$setinv(inv)
    # Return it
    return(inv)

