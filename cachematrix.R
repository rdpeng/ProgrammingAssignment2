## Put comments here that give an overall description of what your
## functions do

# The functions makeCacheMatrix creates a matrix
# that can cache the inverse of the matrix and 
# cacheSolve calculates the inverse
# of the matrix if the inverse is not cached 
# Both the functions assume that the matrix passed is invertible.

## Write a short comment describing this function

# This function receives a matrix as an input and returns a cache matrix i.e.,
# returns a list of 4 objects: set - sets the value of the matrix,
# get - displays the value of x, setinverse - sets the inverse of the matrix,
# getinverse - displays the inverse of the matrix. This function is used to 
# cache the inverse of the matrix along with the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) x <<- y
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# This function receives the matrix created by makeCacheMatrix
# as the input along with other arguments to be passed to the solve function
# and it first checks if the inverse matrix is cached. If it is cached,
# then it returns the inverse matrix or else it calculates the inverse of the
# matrix using R's solve function and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached inverse")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}