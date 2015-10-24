######################################################
##Matrix inversion can be a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly. 
##Below are 2 functions that cache the inverse of a matrix:
##      "makeCacheMatrix":  creates a special "matrix" object that can cache its inverse.
##      "cacheSolve": computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##                    If the inverse has already been calculated (and the matrix has not changed), 
##                    then the cachesolve retrieves the inverse from the cache.
## Code written 24.10.2015 by FV
#######################################################


# The function makeCacheMatrix returns a list of 4 functions that (i) set the matrix to be inversed
#(ii) get the matrix to be inversed, (iii) set the inverse of said matrix, and (iv) get the inverse
# x represents a matrix assumed to be invertible
#`<<-` assigns a value to an object in an environment different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
        my_inv <- NULL
        set <-function(y) {
                x <<- y
                my_inv <<- NULL
        }
        get <-function() x
        setinv <-function(inverse) my_inv <<- inverse 
        getinv <- function() my_inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#The function cacheSolve is written in a way that if the inverse has already been calculated 
#it is retrieved directly from the cache.If not, the inverse is computed and 
#and saved in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        my_inv <- x$getinv()
        
        if (!is.null(inv)){# if the inverse has already been calculated
               
                message("getting cached data")
                return(my_inv)
        }
        
        # if not
        my_matrix <- x$get()
        my_inv <-solve(my_matrix, ...)
        
        x$setinv(my_inv)
        
        return(my_inv)
}