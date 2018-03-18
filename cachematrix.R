## Put comments here that give an overall description of what your
## functions do

## create a vector which is a list containing a function to set and get the value of the vector and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
}
    get <- function() x
    setsolve <- function(inverse) i <<- inverse
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## to get the matrix used in makeCacheMatrix and calculate its inverse

cacheSolve <- function(x, ...) {
    i <- x$getsolve()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
    else{data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
	}
}
