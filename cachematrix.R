## These two functions will inverse the inputted matrix using 
## a unique matrix which will be produced.

## This functiion will create the unique matrix which allows
## the inputted matrix to be inversed

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(inverse) inv <<- inverse
        getinvmatrix <- function() inv
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## This function will inverse the inputted matrix
## using the unique matrix which was produced in the earlier function
## it also checks whether the matrix has been inversed or not
## if yes then computation is skipped
## if not it then computes the inverse

cacheSolve <- function(x, ...) {
	
	inv <- x$getinvmatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinvmatrix(inv)
        inv

}
