## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv_matrix <- NULL
        get_matrix <- function() x
        get_inv_matrix <- function() inv_matrix
        cache_inv_matrix <- function(cache_matrix) inv_matrix <<- cache_matrix
        list(get_matrix=get_matrix,get_inv_matrix=get_inv_matrix,cache_inv_matrix=cache_inv_matrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_matrix  <- x$get_inv_matrix()
        if (!is.null(inv_matrix)){
                message("Getting inverse matrix from cache")
                return(inv_matrix)
        }
        mymatrix <- x$get_matrix()
        inv_matrix <- solve(mymatrix)
        x$cache_inv_matrix(inv_matrix)
        inv_matrix
}
