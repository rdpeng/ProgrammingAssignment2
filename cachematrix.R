## The "makeCasheMatrix" function creates a special "matrix" 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The "cacheSolve" fuction computes the inverse of the special "matric"
## returned by "makeCacheMatrix" above. If the inverse has already been
## calculated, it gets the inverse from the cache. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                reture(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
