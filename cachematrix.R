## These functions help to cache the inverse of a matrix
## if it has already been computed

## This function makes a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL  ##sets the inverse to be null
    set <- function(y) { ##directly sets the value of a given matrix and its inverse
        x <<- y
        i <<- NULL
    }
    get <- function() x ##gets the value of the matrix
    setinverse <- function(inverse) i <<- inverse ##sets the inverse of the matrix    
    getinverse <- function() i ##gets the inverse of the matrix
    list(set = set, get = get, ## list containing all the the above functions
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheinverse <- function(x, ...) {
    i <- x$getinverse() ##gets the inverse of the matrix
    if(!is.null(i)) { ##checks to see if the value has already been computed, in which case, it directly returns that value
        message("getting cached data")
        return(i)
    }
    data <- x$get() ##if value has not been computed
    i <- solve(data, ...) ##inverse is calculated
    x$setinverse(i) ##the value of the inverse is set 
    i
}
