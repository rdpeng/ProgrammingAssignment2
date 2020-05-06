## Two functions to solve and cache the inverse of a matrix. 

##  Function to store the values of a matrix and the inverse.
        ##  Arguments: x - represents a matrix, will be coerced to one if not already
        ##  Variables: inv - stores the inverse matrix
        ##  set - sets the matrix from the input
        ##  get - retrieves the stored matrix
        ##  setInvMatrix - stores the value of the inverse matrix
        ##  getInvMatrix - retrieves inverse matrix from cache
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(solve) inv <<- solve
    getInvMatrix <- function() inv
    list(set = set, get = get, setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

##  Function calculates the inverse of the matrix.
    ##  Arguments: x - stored results from makeCacheMatrix
    ##  Variables: inv - the inverse matrix
    ##             data - retrives the cached original matrix
cacheSolve <- function(x, ...) {
    inv <- x$getInvMatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInvMatrix(inv)
    inv
}
