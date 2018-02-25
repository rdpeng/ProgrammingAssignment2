## makeCacheMatrix: creating a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

##makeCacheMatrix creates a special "matrix", which  contains a function to
##1，set and get the value of the matrix
##2，set and get the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
                invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invrs <<- solve
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

}


## The following function calculates the inverse of the special "mitrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         invrs<-x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached matrix")
        return(invrs)
    }
    matrix<- x$get()
    invrs<- solve(matrix, ...)
    x$setinverse(invrs)
    invrs
}
