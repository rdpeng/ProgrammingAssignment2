## makeCacheMatrix and cacheSolve functions act together to cache and deliver the inverse of a squared matrix

## makeCacheMatrix function caches an input revertible matrix and creates a list of functions to set Matrix, get the Matrix, set the inverse of the Matrix and get the inverse of the Matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    x_inverse <- NULL
    
    set_matrix <- function(matrix_data) { ## set the matrix x
        x <<- matrix_data
        x_inverse <<- NULL
    }
    
    get_matrix <- function() {x} ## get the original matrix
    
    setinv <- function(solve) { ## set the inversed matrix (of x)
        x_inverse <<- solve 
    } 
    
    getinv <- function() { ## get the inversed matrix 
        x_inverse
    } 
    
    return(list(set_matrix = set_matrix, 
                get_matrix = get_matrix,
                setinv = setinv,
                getinv = getinv))
}

##  cacheSolve function computes the inverse of matrix x, input above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    m <- x$getinv()
    
    if(!is.null(m)) { # check if the inverse of 'x' is in cache.
        message("Getting Cached Data")
        return(m)
    }
    
    data <- x$get_matrix() #If the inverse of 'x' is not in cache, the inverse is calculated in this block.
    
    m <- solve(as.matrix(data))
    
    x$setinv(m)
    
    return(x$getinv())
}
