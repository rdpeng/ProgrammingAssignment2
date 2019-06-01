# 1: makeCacheMatrix: This function creates This function creates a special 
# "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL 
    
    # set the value of the matrix
    set <- function(y) {
        
        x <<- y
        m <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix 
    setinverse <- function(inverse) m <<- inverse
    
    # get the value of the inverse of the matrix
    getinverse <- function() m
    # # The first function, makeCacheMatrix creates 
    # a special "list"
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

# 2:cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse 
# has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #read cached data
    m <- x$getinverse()
   
    # check the cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # because cached data is null, calculate the value of x.
    data <- x$get()
   
    # calculate the value of the inverse of x and set 
    m <- solve(data, ...)
   
    # set the value of the inverse of x and the value has been cached.
    x$setinverse(m)
    m
}
