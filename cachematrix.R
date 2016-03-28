## This function creates a  special "matrix" (makeCacheMatrix) object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
## setting the value of the matrix   
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## getting the value of the matrix   
    get <- function() x
## setting the value of inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
## getting the value of inverse of the matrix
 getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}
