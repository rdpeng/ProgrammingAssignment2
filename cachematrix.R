## i have different means of submitting this assignment, they didn't just work, this is my last resort.

## makeCacheMatrix () function creates a special "matrix" object that can cache its inverse.
 

## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
## the get () function returns the matrix x stored in the main function,
## while the set () function changes the matrix stored in the main function.
## the setinverse and getinverse functions are very similar to set and get.
## They don't calculate the inverse, they simply store the value of the input in a variable m.
## into the main function makeCacheMatrix (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function “cacheSolve” computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache. If the inverse has not been calculated, data gets the matrix stored 
## with makeCacheMatrix, m calculates the inverse, and x$setinverse(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

