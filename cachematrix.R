## The function makeCacheMatrix create a special "matrix" which is really a list containing
## a function

## A function to set the matrix, get the matirix, set the inverse of the matrix and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set,get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The function cacheSolve calculates the inverse of the "matrix" created with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
