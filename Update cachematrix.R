makeCacheMatrix <- function(x = matrix()) {
        inmtx <- NULL
        set <- function(y) {
                x <<- y
                inmtx <<- NULL
        }
        get <- function() x
        setinmtx <- function(inverse) inmtx <<- inverse
        getinmtx <- function() inmtx
        list(set = set, get = get,
             setinmtx = setinmtx,
             getinmtx = getinmtx)
}

cacheinverse <- function(x, ...) {
        inmtx <- x$getinmtx()
        if(!is.null(inmtx)) {
                message("getting cached data")
                return(inmtx)
        }
        data <- x$get()
        inmtx <- solve(data, ...)
        x$setinmtx(inmtx)
        inmtx
}
