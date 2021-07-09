## This two functions computing inverse matrix, but before start computation 
## function "cacheSolve" check cache for value of inverse matrix.

## This function "makeCacheMatrix" create a list of function, that set and get 
## matrix, and set and get inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMat) n <<- inverseMat
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This "cacheSolve" function computing inverse matrix, first checks value from
## cache. 

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}

