makeCacheMatrix <- function(x = numeric()){
    m <- NULL
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get the value of the matrix
    get <- function(){x} 
    #set the value of the mean
    setsolve <- function(solve) {m <<- solve}
    #get the value of the mean
    getsolve <- function() {m}
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
    m <- x$getsolve()

    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

#Below is an example
#m1 <- matrix(1:4,2,2)
#x <- makeCacheMatrix(m1)
#x
#m2 <- cacheSolve(x)
#m2
#m1 %*% m2
