## Author: Adrián Santiago
## functions do: Return the inverse of matrix and save the result in cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

# Example:
# > example_matrix <- makeCacheMatrix(matrix(c(5, 3, 2, 4), 2, 2))
# > example_matrix$get()
#      [,1] [,2]
# [1,]    5    2
# [2,]    3    4
# > example_matrix$getInverse()
# NULL
# > cacheSolve(example_matrix)
#            [,1]       [,2]
# [1,]  0.2857143 -0.1428571
# [2,] -0.2142857  0.3571429 
# > cacheSolve(example_matrix)
# getting cached data
# [,1]       [,2]
# [1,]  0.2857143 -0.1428571
# [2,] -0.2142857  0.3571429
# > example_matrix$getInverse()
#            [,1]       [,2]
# [1,]  0.2857143 -0.1428571
# [2,] -0.2142857  0.3571429