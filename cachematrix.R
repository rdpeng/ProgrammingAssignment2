## Cache Matrix and Cache Solve Functions
## 

## Cache Matrix Fuction, 

makeCacheMatrix <- function(x = matrix()) {  ##Initialize a matrix##
    m <- NULL
    set <- function(y) {  ##Define Set Function##
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache_inv <- function(solve) m <<- solve
    getcache_inv <- function() m
    list(set = set, get = get,               ##return a list##
         setcache_inv = setcache_inv,
         getcache_inv = getcache_inv)

}


## Function below to use Solve to get inverse, if not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getcache_inv()
    if(!is.null(m)) {                    ##Check to see if we have object##
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setcache_inv(m)
    m
    
}
