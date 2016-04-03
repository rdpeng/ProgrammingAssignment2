makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMi <- function(solve) m <<- solve
        getMi <- function() m
        list(set = set, get = get,
             setMi = setMi,
             getMi = getMi)
}
cacheSolve <- function(x, ...) {
        m <- x$getMi()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMi(m)
        m
}

test <- makeCacheMatrix(matrix(runif(9,1,100),3,3))