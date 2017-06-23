## In this function I am retrieving a matrix in a variable 'x' and retrieve its length which gives the number of rows and columns 
## This is followed by storing the matrix in a cache. 

makeCacheMatrix <- function(x = matrix()) {
        length(x) <- n
        setMatrix <- function(y) {
                A <- matrix(y,nrows=n/2,ncol=n/2, byrow=TRUE)
                }
                B <<- A
}


## In this function we retieve the matrix and inverse it and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x)){
            message ("Getting Cached data....") 
            return(x)    
                }
        C <- get("B")
        D <<- solve(C)
        D
}
