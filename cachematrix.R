## Put comments here that give an overall description of what your
## functions do

##The function makeCacheMatrix allows the storage in a list, of a matrix and its inverse
##

## Write a short comment describing this function
##usage : a = makeCacheMatrix(matrix(c(1,2,3,4),nrow=2))
##a$get()
##returns the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv #Storage of the calculated inverse in a global variable m
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
  
}


##Only calculates the inverse if the result has not been stored in the list object
##created by the previous function makeCacheMatrix
##cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
       message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)     ##Calls the line : setinv <- function(inv) m <<- inv : of the previous script
    m
}
