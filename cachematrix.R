## The  function makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

# Test Cases below (Thanks to Gregory D. Horne)
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# cacheSolve(amatrix)   # you get a cached copy of the same inverse

makeCacheMatrix <- function(x = matrix()) {
               
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "vector" created with makeCacheMatrix function above.
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
#from the cache using getinverse and skips the computation. Otherwise, it calculates the inverse of the data using solve function
# and sets the value of the mean in the cache via the setinverse function.
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
