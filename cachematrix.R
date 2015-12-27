## compute the inverse of the matrix and cache the result to save recalculating later

## makeCacheMatrix()
## get - Returns the matrix x
## set - Sets matrix x with parameter
## getinv - gets the inverse of the matrix x
## setinv - sets the inverse of the matrix x to inverse parameter


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
   set <- function(y) {
            x <<- y
            inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get,
setinv = setinv,
getinv = getinv)
}


## cacheSolve()
## Calls the already computed mean through getinv()
## If it is NULL, the it gets the matrix, then computes the inverse
## and sets the inverse through setinv
## Else, it returns the cached value

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinv(inv)
inv
}
