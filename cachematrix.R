## The overall purpose of the functions for this assignment is to save 
## processing time by caching results of a function and looking to see
## if that result has already been produced previously.  If it has, it
## returns the cached value, and if not it will run the function.  In 
## this case, the function is to create an inverse of the matrix.

## The makeCacheMatrix function does several things.  First, is that it 
## initializes two objects, x and inv. Then it defines the set function
## and assigns x and inv to the parent environment.  Next, it defines the
## getter, the setter, and the getter of the inverse.  Finally, each of
## these functions is assigned as an element of a list.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The purpose of the cacheSolve function is to return the inverse of
## the matrix.  However, it first checks to see if it has already been
## calculated and stored in cache.  If it has it will return that matrix, 
## otherwise it will move on in the function and calculate the inverse
## using the solve function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Retrieving cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
