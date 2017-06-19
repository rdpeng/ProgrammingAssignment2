### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

Write the following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

# This function creates a matrix object that caches its inverse
makeCacheMatrix <- function(x=matrix()) {
        i <- NULL
        set <- function(z) {
                x <<- z
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function calculates the inverse of the matrix and checks if the inverse has been calculated. If yes, the chached solution is retrieved.

cacheSolve <- function(x,...){
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

# test functions
mat <- makeCacheMatrix(matrix(2:5,2,2))
mat$get()
# inverse is not cached yet
cacheSolve(mat)
# now get cached solution
cacheSolve(mat)


