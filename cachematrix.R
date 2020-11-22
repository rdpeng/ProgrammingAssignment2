## Cache for a matrix. Stores the matrix and it's inverse
## Getters and setters for the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(the_inv) inv <<- the_inv
    
    get_inverse <- function() inv
    
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## Given a cache matrix, returns its inverse.
## The cache is looked up first. If there is a cache miss,
## then the inverse is calculated
## The caller has to ensure that the matrix is invertible

cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    
    if(!is.null(m)) { #Cache hit
        
        message("Getting cached data")
        
    } else {  #Cache miss
        
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        
    }   
    
    m 
    
}
