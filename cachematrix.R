## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        y <- NULL
        setMatrix <- function(s){
                m <<- s
                y <<- NULL
                }
        getMatrix <- function(){
                m
                }
        setInverse <- function(inverseMatrix){
                y <<- inverseMatrix
                }
        list(setMatrix=setMatrix,
             getMatrix=getMatrix,
             setInverse=setInverse,
             getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("get the cached inverse")
                return(m)
                }
        data <- x$getMatrix()
        m <- solve(data)
        x$setInverse(m)
        m
}
