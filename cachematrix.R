## Programming Assignment 2
## This function creates a special "matrix" object to cache the inverse of a matrix, so that
## the inverse can be used in subsequent calculations, which is more efficient than 
## recalculating the inverse of the matrix for each iteration of a loop.  The function will:
## 1) set the value of the matrix, 2) get the value of the matrix
## 3) set the value of the inverse, 3) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This 2nd function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv       
}
