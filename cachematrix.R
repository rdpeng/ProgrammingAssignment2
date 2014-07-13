## [cache the inverse of a matrix]:
## Calculating the inverse of a matrix is potentially time-consuming, 
## especially for a matrix with large size (dimension).
## If the contents of the matrix are not changing, we can cache the inverse of
##  the matrix so that when needed, it can be the cache rather than computed. 
## [functions]:
## makeCacheMatrix && cacheSolve
##----------------------------
#
## makeCacheMatrix creaets a speical "matrix" (in the form of a list that contains all the information) 
## [sub-functions]
## set: set the value of the matrix 
## get: get the value of the matrix
## setinv: set the value of the inverse (of the matrix)
## getinv: get the value of the inverse (of the matrix)
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initialization of the inverse
        set <- function(y) {
                x <<- y        ## <<- send x to the cache environment
                inv <<- NULL   ## <<- send inverse as well
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  ## function returns a list -- special "matrix"

}
#
##----------------------------
#
## Return a matrix that is the inverse of 'x':
## cacheSolve calculates the inverse of the special "matrix" created in the above matrix.
## 1. Check-->If the inverse has already been calculated, it gets the inverse from the cache.
## 2. Else, it calculates the inverse of the given data and sets the value of the inverse in the cache via...
##    setinv function.
#
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {     ## check if the "matrix" has been cached
                message("getting cached data")
                return(inv)
        }
        data <- x$get() 
        inv <- solve(data, ...)
        x$setinverse(inv)       
        inv                     ## return the inverse of the "matrix"
}
