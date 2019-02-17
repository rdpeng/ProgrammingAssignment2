## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## define the argument with default mode of "matrix"
        m <- NULL## initialize inv as NULL; will hold value of matrix inverse
        set <- function(y) {## define the set function to assign new 
                x <<- y		## value of matrix in parent environment
                m <<- NULL	## if there is a new matrix, reset inv to NULL
        }
        get <- function() x	## define the get fucntion - returns value of the matrix argument
        setinverse <- function(inverse) m <<- inverse  ## assigns value of inv in parent environment
        getinverse <- function() m	## gets the value of inv where called
        list(set = set, get = get,
             setinverse = setinverse,	## you need this in order to refer 
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
