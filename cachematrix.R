## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix initializes and prepares the Matriz for caching, testing if one already exists in the current cache of the environment

makeCacheMatrix <- function(x = matrix()) {
        ## Resetting and intializing the Matrix
        invertedMatrix <- NULL
        set <- function(y) {
                ## Defining the function to assign new value of the matrix in parent
                x <<- y
                ## Testing for existing inverted Matrix, in case it exists it will be resetted
                invertedMatrix <<- NULL
        }

        ## Defining the function and getting the value of the matrix's argument
        get <- function() x

        ## Assigns value of inv in parent environment
        setinverse <- function(inverse) invertedMatrix <<- inverse

        ## Getting the value of ivertedMatrix where the call was made
        getinverse <- function() invertedMatrix

        ## Referring to the functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve receives the Matrix and inverts it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                inv
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
