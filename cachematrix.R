## Put comments here that give an overall description of what your
## functions do

## We can input a matrix to the function of makeCacheMatrix.  The matrix is
## the fixed variable.  There is a free variable i in both functions.  The free
## variable stores the inverse matrix value or nothing in the global environment.  


## Write a short comment describing this function
## After inputting the matrix, the function assign NULL to the free variable i. 
## "set" a function stores the input matrix in the global environment.
## "get" is a function returns the matrix.
## "setinverse" assigns the return, which is the inverse matrix, from the
## "cacheSolve" to the global envirnment.
## "getinverse" returns the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## Write a short comment describing this function
## The function first point to the global environment to check there is 
## any inverse matrix exist for the input matrix.
## If yes, return the inverse matrix from the global environment
## If not, "solver" function is applied to the input matrix.  The result
## is store to the free variable i in the global environment. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("geting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
        ## Return a matrix that is the inverse of 'x'
}
