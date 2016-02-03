## These functions allows you to cache and find the inverse of a matrix
## These are to help save time since matrix inversion can be very time consuming
## Note this only works for square matrices whose determinant is nonzero


## This function 'makeCacheMatrix' will make a list containing a function 
## that sets and gets the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() invrs
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function 'cacheSolve' will give the inverse of a matrix that has been cached
## If the inverse has already been computed then it will give the message
## 'Getting cached data...' then reshow the inverse 
## Otherwise it will compute the inverse

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("Getting cached data...")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data)
        x$setinverse(invrs)
        invrs

}

## Test run: 
## > x <- matrix(c(1, 3, 5, 7, 2, 6, 8, 4, 9), nrow=3, ncol=3)
## > A <- makeCacheMatrix(x)
## > A$get()
## [,1] [,2] [,3]
## [1,]    1    7    8
## [2,]    3    2    4
## [3,]    5    6    9
## > cacheSolve(A)
## [,1]      [,2]      [,3]
## [1,] -0.6666667 -1.666667  1.333333
## [2,] -0.7777778 -3.444444  2.222222
## [3,]  0.8888889  3.222222 -2.111111

## When done for a second time
## > cacheSolve(A)
## Getting cached data...
## [,1]      [,2]      [,3]
## [1,] -0.6666667 -1.666667  1.333333
## [2,] -0.7777778 -3.444444  2.222222
## [3,]  0.8888889  3.222222 -2.111111

## Which matches the inverse of our matrix 
## > solve(x)
## [,1]      [,2]      [,3]
## [1,] -0.6666667 -1.666667  1.333333
## [2,] -0.7777778 -3.444444  2.222222
## [3,]  0.8888889  3.222222 -2.111111

