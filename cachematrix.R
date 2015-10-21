## Below are two functions that are used to create a special object that stores a matrix and cache the inverse of it

## The first function creates a list containing a function to set/get the value of the matrix, and set/get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The second function calculates the inverse of the matrix. It first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix, and sets it in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInv(m)
        m
}
