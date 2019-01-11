## These two functions are used to set, calculate, and/or retrieve the inverse of a simple matrix. A calculated matrix inverse (inv) will be cached, so that it can be recalled using the cacheSolve function. If a new matrix is loaded, the cache is erased.

## The makeCacheMatrix function creates a 'matrix' object containing four separate functions: set(), get(), setinv(), and getinv(), alongside a user-defined inversible input matrix (mat), and variable y.

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) mat <<- inv
    getinv <- function() mat
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function takes a 'makeCacheMatrix' type object containing the simple matrix and four formulas to calculate the matrix's inverse and store it in the cache (mat) for future use. If cacheSolve is called upon again with the same inputted matrix, the cached inverted matrix will be retrieved without having to recalculate.

cacheSolve <- function(x, ...) {
    mat <- x$getinv()
    if(!is.null(mat)) {
        message("getting cached inversed matrix")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
}

