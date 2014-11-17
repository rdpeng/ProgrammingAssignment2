## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will first take a square matrix argument as input.
## Then it will define the set, get, setinverse, getinverse functions
## for this matrix as the user can then access this object in another
## function and uses the object's function. The output of this function
## is a list that contains these functions.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
        x <<- y
        inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(Inverse) inverse <<- Inverse
        getinverse <- function() inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function
## This function will take the output from above function,
## which is the special list that contains the matrix object's
## functions. The output of this function will be the inverted 
## matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverted matrix")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
