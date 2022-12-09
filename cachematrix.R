## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a matrix object that caches its inverse making it an invertible square matrix.
makeCacheMatrix <- function(x = matrix()) {             ## Defines the argument as a matrix
    i <- NULL                                           ## Creates i as the variable set to default as NULL (matrix inverse value)
    set <- function(y) {                                ## Define the set function to change cached values of a matrix in 
        x <<- y                                         ## parent environment
        i <<- NULL                                      ## In case of new matrix, resets i to NULL
    }
    get <- function() x                                 ## Get function returns the value of the matrix arguments.
    setinverse <- function(inverse) i <<- inverse       ## <<- operator assigns value of i in parent environment
    getinverse <- function() i                          ## Gets the value of i where called 
    list(set = set,                                     ## Required for $ operator functions to reference
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the "matrix" returned by makeCacheMatrix function defined above.
## If the inverse has been calculated with no changes to the matrix cacheSolve returns the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()                         ##Returns matrix that is the inverse of X
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
