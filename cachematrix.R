## Matrix Inverse Caching:
## The functions below allow you to save processing time in R when finding
## the inverse of a matrix or series of matrices using caching. These functions
## save memory and improve calculation speed because the matrix inverse is 
## cached.

## makeCacheMatrix() takes a matrix 'x' as argument and makes a list of 
## functions

makeCacheMatrix <- function(x = matrix()) {
        B <- NULL
        set <- function(y){
                x <<- y
                B <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) B <<- inverse
        getInverse <- function() B
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

##cacheSolve() prints a cached inverse matrix; the inverse of original matrix

cacheSolve <- function(x, ...){
        B <- x$getInverse()         
        if(!is.null(B)){            
                message("getting cached data")
                return(B)
        }
        data <- x$get()
        B <- solve(data, ...)
        x$setInverse(B)
        B
}

x <- matrix(c(-4, 0, 0, -4), 2, 2)    #Create the original matrix 'x' here.
myMatrix <- makeCacheMatrix(x)
cacheSolve(myMatrix)