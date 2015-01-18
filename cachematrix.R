## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # initialize m
        set <- function(y) {
                x <<- y       # set x to y's value in ______ environment
                m <<- NULL    # set m to NULL, solution to inverse equation x %*% m = 0, in ______ environment
        }        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse       # set setmean in get's envir and m to inverse in ____ envir
        getinverse <- function() m                    # set getinverse to cached value if exists
        list(set = set, get = get
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {        # test if m already exists (TRUE), will get cached inverse
                message("getting cached data")        # advising that inverse already calc'd and retrieving from cache
                return(m)        # returning cached inverse
        }
        data <- x$get()        # if inverse not in cache, getting data to solve inverse
        m <- solve(data, ...)        # calculating inverse
        x$setinverse(m)        # putting inverse in cache
        m                      # returning inverse
}        



