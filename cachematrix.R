## The following functions (makeCacheMatrix and cacheSolve) are used to
## calculate the inverse of a matrix. If the matrix has an inverse, it will 
## work. The main purpose of these functions are to stores (cache) the value
## of the inverse if the elements in the matrix do not change, so calculate 
##it again would be unnecessary.

## makeCacheMatrix: contains a list of functions (4) to set and get the 
##matrix and to get and set (by solving it) the value of the inverse of 
##the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: this function checks if the value of the inverse has been
##previously calculated. Otherwise, it solves and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
