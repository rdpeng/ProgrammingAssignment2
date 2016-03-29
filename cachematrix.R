## This is all about caching the inverse of a matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## x should be invertible matrix.
        inv <- NULL
        set <- function(y) {
                ## "<<-" is superassignment operator and used to assign a value
                ## to an object in an environment that is different from the 
                ## current environment.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## if the inverse has already been calculated
        if(!is.null(inv)) {
                # get it from the cache 
                message("getting cached data")
                return(inv)
        }
        ## otherwise, the inverse should be calculated.
        matrix <- x$get()
        inv <- solve(matrix, ...)
        ## Set the value of the inverse in the cache.
        x$setinverse(inv)
        inv
}

## A couple of examples
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getinverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getinverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getinverse()
