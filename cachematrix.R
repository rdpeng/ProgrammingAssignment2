## These functions together have as result the inverse of a matrix as per the follow example:
##> myMatrix<-c(1,3,5,8)
##> dimension(myMatrix)<-c(2,2)
##> cacheSolve(makeCacheMatrix(myMatrix))
##           [,1]       [,2]
##[1,] -1.1428571  0.7142857
##[2,]  0.4285714 -0.1428571

## The function below creates a matrix that can be inversed and cached. 
## As per instuction it is assumed that is is possible to inverse the given function and no error
## trapping has been included.

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
        set <- function(y){
                 x <<- y
                 m <<- NULL
        }
        get  <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This returns the inverse of matrix "x" calulated above.
## If the inverse has already been calculated and is found in the cache, the version in the cache is returned.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cashed data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse
        m
}
