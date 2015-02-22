## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 ## set the value of the vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
 ## get the value of the vector
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
 ## matrix is a function and  solve need a matrix.
 ## so we need to change matrix<-x$get to matrix<-x$get()
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
