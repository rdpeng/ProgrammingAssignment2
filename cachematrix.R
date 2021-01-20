## Put comments here that give an overall description of what your
## functions do

##This function will create a matrix object 

makeCacheMatrix <- function(x = matrix()) {
 x <- NULL
        ## seters and geters
     setm <- function( m ) {
            matrix <<- m
            x <<- NULL
    }      
   getm <- function() {
    	matrix
    }
    seti <- function(i) {
        x <<- i
    }
     geti <- function() {
        ## Return the inverse property
        x
    }
        
}


## this function computes the inverse of the matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    s <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(s) %*% s

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
