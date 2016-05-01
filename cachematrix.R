# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inv <<- solve
        get_inverse <- function() inv
        list (set=set, get=get,
              set_inverse=set_inverse, get_inverse=get_inverse)

}


# The following function returns the inverse of the matrix, but first it
# searches if the inverse has already been computed.


cacheSolve <- function(x, ...) {
       inv <- x$get_inverse()
       if(!is.null(inv)) {
               message("getting cached data.")
               return(inv)
       }
       matrix <- x$get()
       inv <- solve(matrix,... )
       x$set_inverse(inv)
       inv
}
