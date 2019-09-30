## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         inversematrix <- NULL
        setmatrix <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        #get value of matrix
        getmatrix <- function() x
        #assign value of inverse matrix
        setinverse <- function(inverse) inversematrix <<- inverse
        #get value of inverse matrix
        getinverse <- function() inversematrix
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("getting cached inverse matrix")
                return(inversematrix)
        }
        data <- x$getmatrix()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix

}
