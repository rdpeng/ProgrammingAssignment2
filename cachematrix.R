## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        
        set <- function(y){
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) mat_inv <<- inv
        getInv <- function() mat_inv
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}


