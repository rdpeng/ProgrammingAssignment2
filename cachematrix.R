## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will:
## set the value of the matrix, 
## get the value of the matrix
## set the value of inverse matrix, 
## get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<-solve
        getinverse <-function () m
        list(set=set, get=get,setinverse =setinverse, getinverse=getinverse)
}


## cacheSolve calculates the inverse of the matrix created with above function.
## It first check if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skip the computaion.
## Otherwise it calculates the inverse matrix through the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getinverse()
        if(!is.null(m)) {
                message("retrieving cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

