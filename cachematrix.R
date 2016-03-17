## The following functions allow for the inverse of a matrix to be
## cached eliminating the need to compute the inverse every single
## time.


## This function essentially creates a list containing four functions
## that set the value of a matrix, get the value of a matrix,
## set the value of the inverse of the matrix and get the value of
## inverse of the matrix.

## Note: This function assumes that the matrix is invertible.
makeCacheMatrix <- function(x = matrix()) {
       
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
      
}



## This function first checks to see if the inverse of the matrix
## is already computed and returns the cached inverse. If a cached
## inverse of the matrix is not available, it computes the inverse.

## Note: This function assumes that the matrix is invertible. 
cacheSolve <- function(x, ...) {
      
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("retrieving cached data.")
                return(inverse)
        }
        m <- x$get()
        inverse <- solve(m)
        x$setinv(inverse)
        inverse
}
