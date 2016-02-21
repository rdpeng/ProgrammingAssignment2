## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinv_matrix <- function(solve) inv_matrix <<- solve
        getinv_matrix <- function() inv_matrix
        list(set = set, get = get, 
             setinv_matrix = setinv_matrix,
             getinv_matrix = getinv_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinv_matrix()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setinv_matrix(inv_matrix)
        inv_matrix
}
