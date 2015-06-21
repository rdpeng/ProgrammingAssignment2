
## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly

## the first function, makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    } 
    get <- function() x
    setinverse <- function(inverse)  i <<- inverse
    getinverse <- function() i  
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the matrix.
## However, it first checks to see if the inverse has already returned
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix by using solve function.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }   
    data <- x$get()
    i <- solve(data,...)  
    x$setinverse(i)
    i
}
