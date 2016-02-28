## my functions create a special object that stores a matrix and caches its inverse

## the first part sets and gets value of the matrix; sets and gets value of inverse of the matrix
## similar to the vector mean function in the example

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## the following function calculates the inverse of the matrix created above. 
## first, it checks whether the inverse has been calculated. If so, skip computation; if not, continue calculation
## Added "ginv" function to ensure that the inverse can still be calculated even if the matrix is not square as assumed:)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    if(dim(data)[1]== dim(data)[2])
    {m <- solve(data)}
    else
    {   library(MASS)
        m<-ginv(data)}
    x$setinverse(m)
    m
}
