#function makeCacheMatrix creates a special "matrix" 
# object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() {x}
        set_inverse <- function(solve) {matrix <<- solve}
        get_inverse <- function() {matrix} #returns NULL if matrix wasn't cached
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

#function cacheSolve returns the inverse of the matrix which 
# we make by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$get_inverse()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        y <- x$get()
        matrix <- solve(y, ...)
        x$set_inverse(matrix)
        matrix
}
