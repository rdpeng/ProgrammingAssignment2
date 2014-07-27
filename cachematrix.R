## This is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinver <- function(solve = matrix()) m <<- solve
        getinver <- function() m
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)



}




##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        m <- x$getinver()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinver(m)
        m

       
        ## Return a matrix that is the inverse of 'x'
}
