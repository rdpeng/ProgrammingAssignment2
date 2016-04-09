## The makeCacheMatrix function creates a special matrix object that can cache its inverse
## then use the cacheSolve function to compute the inverse of the special matrix that returned
## by the makeCacheMatrix. If the inverse is already calculated and the matrix has not changed,
## then the cacheSolve() retrieve the inverse from the cache then returns it otherwise it 
## calculates the inverse

## makeCacheMatrix contains 4 functions: set, get, setInv, and getInv

makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL
    	set <- function(y) {
             x <<- y
             inverse_x <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inverse_x <<- inverse
    getInv <- function() inverse_x
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve() returns the inverse of the matrix from makeCacheMatrix().
## If the inverse is calculated, it retrieves then returns. Otherwise, it calculates
## the inverse using the solve()

cacheSolve <- function(x, ...) {
        inverse_x <- x$getInv()
    if (!is.null(inverse_x)) {
        message("getting cached inverse matrix")
        return(inverse_x)
    } else {
        inverse_x <- solve(x$get())
        x$setInv(inverse_x)
        return(inverse_x)
}
