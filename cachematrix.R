# 1: makeCacheMatrix: This function creates This function creates a special 
# "matrix" object that can cache its inverse.

# 2:cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


# The first function, makeCacheMatrix creates 
# a special "vector", which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix 
# get the value of the inverse of the matrix
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


# The second function calculates the inverse of matrix,
# but at first it first checks to see if the inverse has 
# already been calculated. If so it gets the inverse from
# the cache and skips the computation.
# Otherwise, it calculates the inverse and set the inverse
# in the cache via the set inverse function.
cacheSolve <- function(x, ...) {
        
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
