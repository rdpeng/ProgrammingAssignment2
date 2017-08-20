## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinvesre)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvesre(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
