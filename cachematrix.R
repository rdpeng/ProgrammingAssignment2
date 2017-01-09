## This code will cache the inverse of a matrix so we don't have to compute it every time.

## makeCacheMatrix takes a matrix as it's input and returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {

}


## cacheSolve inverts the matrix created with the above function. It first checks to 
## see if the inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets it in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
