## The first function, makeCacheMatrix creates a special "matrix"
## The function  is used to set and get the value of vector,and 
## set and get the value of the inverse of the special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function checks to see if the inverse has already
## calculated from above function and gets the inverse from the
## cache. If not, it calculates the inverse via 'solve' and set
## the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}