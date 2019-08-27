## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse. 
##cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.

## Code for makeCacheMatrix

makeCacheMatrix <- function(mat = matrix()) {
        c1 <- NULL
        create <- function(mat1) {
                mat <<- mat1
                c1 <<- NULL
                }
        
        show <- function() {
                mat
                }
        
        create_inverse <- function(inverse) {
                c1 <<- inverse
                }
        
        show_inverse <- function() {
                c1
                }
        
        list(create = create, show = show, create_inverse = create_inverse, show_inverse = show_inverse)
}


## Code for cacheSolve

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        c1 <- mat$show_inverse()
  
        if (!is.null(c1)) {
                return(c1)
                }
        
        d <- mat$show()
        c1 <- solve(d, ...)
        mat$create_inverse(c1)
        c1
}
