## making an inverse cache of a matrix in hopes of improving speed when doing
##calculations involving a matrix

## making an inverse cache of a matrix

makeCacheMatrix <- function(x = matrix()) {
        o <- NULL
        set <- function(y){
                x << - y
                o << - NULL
        }
        get <- function()o
        setinverse <- function(inverse)o <<- inverse
        getinverse <- function()o
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## returning a matrix that is the inverse of 'x' from cache

cacheSolve <- function(x, ...) {
        t <- x$getinverse()
        if (!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        v <- solve(data,...)
        x$setinverse(v)
        v
}
