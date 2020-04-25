## cost computational and used for matrix inverse caching also
## functions do

## function creates a special matrix that can inverse the cache

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setmean <- function(mean) m <<- mean
       getmean <- function() m
       list(set = set, get = get,
            setmean = setmean,
            getmean = getmean)
}


## to solve the cache matrix this is used
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- mean(data, ...)
       x$setmean(m)
       
}
