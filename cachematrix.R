## These functions are used to earn time and memory usage when calculating the reverse of a Matrix. If the reverse matrix
## has already been calculated before these functions recall the result from the cache data instead of calculating
## the reverse matrix again. 

## In this function we create a list with functions wich will be recalled by cacheSolve function.
## There are four elements for setting and getting the original matrix and two other functions for setting and getting
## the reverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setrev <- function(solve) m <<- solve
        getrev <- function() m
        list(set = set, get = get, setrev = setrev, getrev = getrev)
}


## In this function we recall the reverse matrix, if it has been already calculated it returns teh cache data stored value.
## If not, it stores as data the original matrix, calculates the reverse (solve function) and stores it to the cache memory.
## Finally it recalls the reverse matrix.

cacheSolve <- function(x, ...) {
               m <- x$getrev()
        if(!is.null(m)){
                message("getting cached data")
                return(m)        
        }
        data <- x$get()
        m <- solve(data,...)
        x$setrev(m)
        m
}
