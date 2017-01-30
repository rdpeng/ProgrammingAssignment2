## Set and get the value of a matrix
## Set and get the value of the inverse of the matrix

## create a matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cache the inverse of a matrix

cacheSolve <- function(x, ...){
        s <- x$getinverse()
        t <- x$get()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        if(s == t) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
