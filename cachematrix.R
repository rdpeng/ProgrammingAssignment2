## First function gets a matrix (x), and return a list of four ##functions to be used in next function. cacheSolve either uses ##an already calculcated inverse, or use the inverse matrix ##calculated before.

## gets a matrix (x), and return a list of four functions to be ##used in next function.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## either uses an already calculcated inverse, or use the inverse ##matrix calculated before.


cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
