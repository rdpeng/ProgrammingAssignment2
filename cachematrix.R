##Caching and inversing a matrix.

makeCacheMatrix <- function(x=matrix()) {
    ## Creates a special "matrix" object that can cache its inverse.
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

cacheSolve <- function(x, ...) {
    ## Computes the inverse of the special "matrix" attained in the makeCacheMatrix function above.
    ## If inverse matrix already exists, this retrieves it from the cache.
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
