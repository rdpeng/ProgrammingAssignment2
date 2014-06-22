## Creates a matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) i  <<- inverse
        getinverse  <- function() i
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## If the inverse has already been calculated (and the matrix has not changed),
## then retrieve the inverse from the cache else compute the inverse

cacheSolve <- function(x, ...) {
        i  <- x$getinverse()
        if (!is.null(i)){
                message("getting cached matrix inverse")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinverse(i)
        i
}