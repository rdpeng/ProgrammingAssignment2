## This file makes the Matrix in inverse the matrix in the cache and leave it in cache. 
## functions do

## This Function the save the inverse Matrix in Memory (cache).

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setMatrix <- function (solve) m <<- solve
        getMatrix <- function () m
        list(set = set, 
             get = get, 
             setMatrix = setMatrix, 
             getMatrix = getMatrix)
}


## This function calculate the inverse of the Matrix and store in Cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getMatrix()
        if (!is.null(m)) {
                message("Get inverse Matrix from cache.")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
