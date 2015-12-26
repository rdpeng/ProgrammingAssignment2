makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <- solve
        getinverse <- function() m 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
