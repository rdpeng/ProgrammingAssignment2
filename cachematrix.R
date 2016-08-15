## These functions will cache the invese of a matrix


## A function to create a special 'matrix' object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set <-function(y) {
                x<<-y
                s<<-NULL
        }
        get <- function()x
        setsolve <- function(solve)s <<- solve
        getsolve <- function() s
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)                    
}


## A function to compute the inverse of the special 'matrix' object


cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrx <- x$get()
        s <- mean(matrx, ...)
        x$setsolve(s)
        s
}
