## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invmat<- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following cacheSolve function will return an inverse of the input matrix. It first checks to see
# if the inverse has already been computed. If it has, it gets the result and skips the rest
# If it hasn't, it computes the inverse of the matirix and sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached data.")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data)
        x$setinverse(invmat)
        invmat
}
