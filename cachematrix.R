## The first function, makeVector creates a special "vector", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        elc <- NULL
        set <- function(y){
                x <<- y
                elc <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) elc <- inverse
        getinverse <- function() elc
        list(set=set, get=get, setreverse=setreverse, getreverse=getreverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(x)
        x$setinverse(inv)
        inv
}
