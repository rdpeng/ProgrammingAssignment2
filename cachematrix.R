# function to create a special matrix object that can cache its inverse
# defines setters & getters for the matrix and its inverse
# Inverse computation is out of scope of this function
#
# No special checks are done if matrix is SQUARE or not - enhancement can be done
#
# Args:
#   x: Matrix object to be cached
#
# Returns:
#   list of setter and getter functions

makeCacheMatrix <- function(x = matrix()) {

    #initialize the inv to null value
    inv <- NULL
    #set the matrix object
    set <- function(f) {
        x <<- f
        inv <<- NULL
    }
    #return the matrix object
    get <- function() x
    #set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    # Get the inverse of the matrix
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# function to compute the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then it retrieves the inverse from the cache
#
# No special checks are done if matrix is SQUARE or not - enhancement can be done
#
# Args:
#   makeCacheMatrix - list containing functions to set/get the matrix & inverse
#
# Returns:
#   inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # check if Matrix inverse is already computed & cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Compute the inverse and cache the results. Assume matrix is inversible
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
