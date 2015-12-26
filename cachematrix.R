## This is the prpgraam to compute the inverse of the matrix and cache the result so as to retreive
## from the cache whenever call to this function is made, thus saving the time for computing the inverse
## everytime.


## makeCacheMatrix()
## Input - matrix x to be created
## Output - list of functions (get, set, getInverse, setInverse)
## get - Returns the data in the matrix x
## set - Sets matrix x with the data passed as parameter
## getinv - gets the inverse of the matrix x
## setinv - sets the inverse of the matrix x through the function parameter/arguement


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
## Input - x, the matrix whose inverse needs to be computed
## Output - the inverse of the matrix x
## Flow: Invokes the already computed mean through getinv()
## If it is NULL, the it gets the matrix using get(), then computes the inverse
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
