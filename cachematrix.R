## Two functions to cache the inverse of a matrix and return the 
## inverse of the matrix. If inverse is cached, user is alerted
## and cached value is returned.

## makeCacheMatrix takes a matrix as argument, stores the vector
## and its inverse. It returns a list of functions, set(), get(),
## setmatrix(), and getmatrix()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}

## cacheSolve must have as argument the list of functions
## produced by makeCacheMatrix(). It returns the inverse
## of the matrix by means of solve(), unless this value
## is already cached. In which case it alerts the user
## and returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}
