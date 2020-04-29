## This code provides two functions, 'makeCacheMatrix' and 'cacheSolve'
## to calculate and store the inverse of a given matrix x.

## This function provides methods to 
## 1) set the value of a given matrix x
## 2) get the value of a given matrix x
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks if the inverse has already been stored and if
## not, it calculates the inverse of the matrix x and caches it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
