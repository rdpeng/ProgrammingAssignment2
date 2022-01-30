## Put comments here that give an overall description of what your
## functions do
# The makeCacheMatrix function creates the matrix object in the same way that the example function creates the vector object
# The difference with the example is that 'setmean/getmean' become 'setinverse/getinverse', and the m variable within the function now holds the inverse of a matric rather than a mean
# The cacheSolve function checks whether the matrix inverse has been cached, and if not it commputes it using the 'solve()' fucntion
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(m) m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# The cacheSolve function checks whether the matrix inverse has been cached, and if not it commputes it using the 'solve()' fucntion

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
