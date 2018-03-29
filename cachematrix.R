##Create a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of the matrix and check whether it has been cached already.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ?       i
        
        ## Return a matrix that is the inverse of 'x'
}
