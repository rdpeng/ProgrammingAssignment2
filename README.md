
### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

Write the following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.


Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.
# cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
            IN <- NULL
            set <- function(y) {
                    x <<- y
                    IN <<- NULL
            }
            get <- function() x
            setinverse <- function(solvematrix) IN <<- solvematrix
            getinverse <- function() IN
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
    }
    
    2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.
    
#function to retreive inverse of matrix
IN <- x$getinverse()
if (!is.null(IN){
message('get cached data')
return(IN)
data <- x$get()
IN <- solve(data)
xsetinverse(IN)
IN
}
