#### Programming Assignment 2 ####

## makeCacheMatrix is a list containing functions to:
## 1. Set the value of Matrix
## 2. Get the value of Matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## sets value of the Matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() {
        x
    }
    
    ## set the value of the inverse
    setinverse <- function(matrix_inverse) {
        m <<- matrix_inverse
    }
    
    ## get the value of the inverse
    getinverse <- function() {
        m
    }
    
    ## return list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of the special "matrix", but first checks 
## if the matrix inverse has already been calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) { ## inverse has already been calculated
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
