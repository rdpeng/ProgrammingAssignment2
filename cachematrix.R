## Week 3 Assignment for Coursera R Programming class

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Set the inverse to NULL
    set <- function(y){
        x<<-y ## Assign the y matrix to the x matrix
        inv <<- NULL ## reset/clear?<<- inv to NULL for a new matrix
}
    get <- function() x ## Return the matrix x
    setinverse <- function (inverse) inv<<-inverse ## assign inverse to inv
    getinverse <- function() inv ## Return the matrix inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() ## set the inv variable. This might be NULL
        if (!is.null(inv)) {
            message("Retrieving cached values") ## If inv already exists then grab it from the cache
            return (inv)
        }
        data <- x$get() ## Get the matrix data
        inv <- solve(data) ## Calculate the inverse
        x$setinverse(inv) ## Set the Inverse into the inv variable
        inv  ## Return the inverse
}
