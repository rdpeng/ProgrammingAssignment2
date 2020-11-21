## Put comments here that give an overall description of what your
## functions do

## this function creates a matrix 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { ##sets value of matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x ##gets value of matrix
    setinverse <- function(inverse) i <<- inverse  ##sets value of inverse
    getinverse <- function() i  ##gets value of inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function returns the inverse of the matrix above

cacheSolve <- function(x, ...) {
    i <- x$getinverse() 
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get
    i <- solve(data, ...)  ##shows the inverse is wanted
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
