##  Submission - Assignment 3 - GitHub ID: DocUrlaub (Coursera ID: Matthew)
##  Description: The .R script "cachematrix.R" contains two functions (makeCacheMatrix / cacheSolve) intended
##  to demonstrate the use of Lexical Scoping within R, in addition to performing the inversion of
##  a matrix passed into the function.  If the matrix passed into the functions has been pre-calculated,
##  then instead of recalculating the inversion of the matrix, the functions will return the previously
##  cached solution contianing the inverted matrix result.


#  Function: makeCacheMatrix - creates a list of functions to be called by the parent function (cacheSolve)
#  , in addition to initalizing the variables i to NULL.  The 4 functions contained in the list are defined herein.
#  Finally all previously calculated results are cached in i, to be returned if the non-inverted matrix is passed again.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {i <<- inverse}
        getinverse <- function() {i}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#  Function: cacheSolve - checks the condition if variable i, and if the value is not NULL, returns the cached result fo the
#  inverted matrix.  If the condition of i is NULL, then the function calls the get() function in order to assign the variable
#  data the matrix value, and then proceeds to invert the matrix and pass the result to i using the setinversion() function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Returning cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
