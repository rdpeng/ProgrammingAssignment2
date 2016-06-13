## Put comments here that give an overall description of what your
## We want to be able to build a function that can invert a number of arguments
## at a given time. 

## Write a short comment describing this function
## the function makeCacheMatrix will generate a special matrix that can
## cache the inverse of the values in matrix form

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m<<- solve
        getmatrix <- function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}



## the cacheSolve function computes the inverse of the matrix that
## is provided by the makeCacheMatrix function.  

cacheSolve <- function(x, ...) {
        m<- x$getmatrix
        if(!is.null(m)){
           message("getting cached data")
           return(m)
        }
        matrix <- x$get()
        m <- solve(matrix,...)
        x$setmatrix(m)
        m
}
