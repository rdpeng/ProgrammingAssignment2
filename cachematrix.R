##Pair of functions that cache the inverse of a matrix

#This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
                invcache <- NULL
                set <- function(y) {
                        x <<- y
                        invcache <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) invcache <<- inverse
                getinverse <- function() invcache
                list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                invcache <- x$getinverse()
                if(!is.null(invcache)) {
                        message("getting cached data.")
                        return(invcache)
                }
                data <- x$get()
                invcache <- solve(data)
                x$setinverse(invcache)
                invcache
}
