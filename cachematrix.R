## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## In makeCacheMatrix, we made inv ( Null value stored at first) object that will store the Inverse of the matrix. Then we initialize,
## set object to the function(y). We set the value of x to the value of matrix in parent environment and If there is a new matrix, we reset inv to Null.
## We define the get function that will return the value of matrix argument. Similarly, we will make the setinverse function to store the value of inv in parent
## environment. To get the value where inv is called we made a getinverse function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)     
}


## Write a short comment describing this function

## This function is made to get the inverse of the 'x', but if the inverse has not been cached, this function caches the inverse and collects the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...)
        x$setinverse(inv)
        inv
}
