##makeCacheMatrix creates a list containing a function to

#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of the matrix
#4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv_m <<- inverse
    getinv <- function() inv_m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


##The following function first checks if the inverse has already been
##computed. If it has been, it gets the result and skips performing
##the computation. If not, it computes the inverse and sets the value
##in the cache using setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinv()
    if(!is.null(inv_m)) {
        message("getting cached data.")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data)
    x$setinv(inv_m)
    inv_m
}
