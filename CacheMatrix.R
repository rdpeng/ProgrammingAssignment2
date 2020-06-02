makeMatrix <- function(x = matrix()) {
        ##write a anonymous function to create a matrix
	##collect data by assigning below variables
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cachesolve <- function(x, ...) {
        ## write a anonymous function to get its inverse
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##use solve function to cache the data
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
