## These functions are used in order to cache the inverse of an matrix. Therefore, when 
## computing the inverse of a matrix, if the inverse is already cached, it does not 
## need to be re-computed. 

## This function is used to cache a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## This function is used to find the inverse of a matrix. 

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
