## I based my functions off of the ones described in the example, only 
## modifying necessary elements and object names.

## The first function clears out previous values for s, which is the inverted
## matrix. Then it creates three functions: get will be used to get the matrix,
## setinverse will be used to store the inverse of the matrix, and getinverse 
## retrieves that value. Then I create a list of functions to draw from in the
## cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
      
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list( get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  The second function utilizes the functions defined prio. First, it pulls s,
##  the solved matrix. If it can find s (!isnull(s) = TRUE) it will just print
##  that value from the last time the function was exexuted. It will print a
##  message so you know that's what happened. Otherwise, it will calculate the
##  inverse and the cache that value as s.


cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
