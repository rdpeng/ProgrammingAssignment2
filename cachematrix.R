#Function responsible for creating and storing inverse matrix version of the input (has to be a inversible square matrix)
#Functions were tested on 3x3 matrix generated with command:
#
#mat <- matrix(rnorm(9), 3, 3)
#
#Functions are called in the following order:
#pre.inv <- makeCacheMatrix(mat)
#cacheSolve(pre.inv)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#Function responsible for either recovering inverted matrix from cache or initiating process of inverting (if no cache existing)
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
