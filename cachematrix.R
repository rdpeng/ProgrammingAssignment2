
# The following two functions calculate the inverse of a matrix.
# To speed up the calculation of the inverse, when a matrix's inverse
# has already been calculated previously, the result will be returned 
# using the stored value in cache instead of doing the calculation again. 

#makeCacheMatrix creates a special "vector", which is really a list containing a function to
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse of the matrix
#    get the value of the inverse of the matrix

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

#The following function calculates the inverse of the special "vector" 
#created with the above function. However, it first checks to see if 
#the inverse has already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, it calculates 
#the inverse of the matrix and sets the reverse in the cache via
#the setinverse function.

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
