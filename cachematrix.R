# The following pair of functions are used to cache the inverse of a matrix.

## This function creates a "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix  
## 2. get the value of the matrix  
## 3. set the value of inverse of the matrix  
## 4. get the value of inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function below computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix is unchanged), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

###**Note:** For this assignment, it is assumed that the matrix given is invertible. 
