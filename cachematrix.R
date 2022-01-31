## The functions below caches the inverse of matrix for reuse in other functions or parts of your code
## The functions first checks that the values of the matrix cache has not changed before reusing. If it has it does the inversion computation 
## again before passing it to the function or code that needs the inverted matrix

## This first function creates a special matrix and through a series of functions sets
## and make a cache of the inverse matrix .This function resets the
## inverse of the matrix to NULL if a new matrix is passed as an argument

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function (y) {
                  x<<- y
                  i <<- NULL
                }
                get <- function () x
                setinverse <- function(solve) i <<- solve
                getinverse <- function () i
                list (set = set, get = get, setinverse = setinverse, 
                      getinverse = getinverse)

}


## The function below first checks to determine if the value of the cached inverse matrix is not NULL,
## if TRUE(yes) , it  skips the inverse matrix computation  and returns the cache inverse matrix to the parent environment.If FALSE (no) 
## the function below will calculate the inverse of the new matrix and return for caching in the parent environment.

cacheSolve <- function(x, ...) {
           i<-x$getinverse()
           if(!is.null(i)) {
                 message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
             return(i)
           }
           data <- x$get()
           i <- solve(data,...)
           x$setinverse(i)
           i
}
