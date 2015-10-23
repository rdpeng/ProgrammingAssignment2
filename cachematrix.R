## Put comments here that give an overall description of what your
## functions do

## Creates a list of fuctions to set the value of the matrix, get the matrix, set the inverse of the matrix in the cache and  get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Uses the makeCacheMatrix function to return the inverse of the matrix.If the inverse has already been calculated, it gets the matrix inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of this in the cache via the setinverse function from the make cachematrix function
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
}



