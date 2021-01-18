## Put comments here that give an overall description of what your
## functions do

## cachematrix.R is a program that allows the user to get the inverse of a 
## matrix by either computing and caching the result, or getting a cached 
## version if the inverse had already been computed.


## makeCacheMatrix() initializes an object of class makeCachematrix which 
## returns 4 functions: 
## -set(): initializes the matrix to be inverted and the inverted matrix 
##         in the makeCacheMatrix object. NOTE: calling the set() function of 
##         this object, reinitializes the the object without creating a new 
##         instance 
## -get(): returns the original matrix passed to makeCachematrix (either with 
##         the initial call tomakeCachematrix() or with set()
## -setinv(): caches the inverse of the matrix  
## -getinv(): returns the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) inv <<- invMat
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## cacheSolve() returns the inverse of a matrix stored in an object of class 
## makeCacheMatrix by either: 
## - retrieving the cached result if the function had already been called on 
##   this matrix
## - computing the inverse if it's the first call of cache Solve() on this 
##   matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    else{
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
    }
}

