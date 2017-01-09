##makeCacheMatrix creates and returns a list of functions
## used by cache to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to null
        m <- NULL
        # matrix created in the working environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the matrix value
        get <- function() x
        # matrix inverted and stored in cache
        setinverse <- function(solve) m <<- solve
        # get the inverted matrix from cache
        getinverse <- function() m
        # return the created function to the working environment
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cachesolve cretes the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix is not present in cache, it is created in the working environment
## and its inverted value is stored in the cache

cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix stored in cache
        m <- x$getinverse()
        # return the inverted matrix from cache if it exists else
        # create the matrix in working environment
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
        }
        #cache matrix since it does not exist
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
