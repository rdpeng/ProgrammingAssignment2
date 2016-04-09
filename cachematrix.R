## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Function crates a vector with three functions
# get_matrix with returns a matrix argument
# get_inv_matrix which retuns the inverted matrix
# cache_inv_matrix which caches the invert matrix

#From the orginal code I removed uncessary lines to make mine more efficient

makeCacheMatrix <- function(x = matrix()) {
        
        inv_matrix <- NULL
        get_matrix <- function() x
        get_inv_matrix <- function() inv_matrix
        cache_inv_matrix <- function(cache_matrix) inv_matrix <<- cache_matrix
        list(get_matrix=get_matrix,get_inv_matrix=get_inv_matrix,cache_inv_matrix=cache_inv_matrix)
        
}


## Write a short comment describing this function

#Function cacheSolve returns an inverted matrix from cache if not in cache then inverts the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # variable to return the invert matrix
        # Test if invert matrix is cached in memory
        # If true then return the matrix from cache
        
        inv_matrix  <- x$get_inv_matrix()
        if (!is.null(inv_matrix)){
                message("Getting inverse matrix from cache")
                return(inv_matrix)
        }
        
        # If the invert matrix is not in cache then calcule the invert matrix
        
        
        mymatrix <- x$get_matrix()
        
        # Function solve() inverts the matrix
        
        inv_matrix <- solve(mymatrix)
        x$cache_inv_matrix(inv_matrix)
        inv_matrix
}
