## This function will cache the inverse of a matrix

## makeCacheMatrix function will create a special matrix which is a list of function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inversed matrix
##get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
        rev <- NULL
        set <- function(y) {
                x <<- y
                rev <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) rev <<- inverse
        getinverse <- function() rev
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will retrieve the inverse of 'x' if it has already been computed
## else it will compute the inverse matrix and save it to cache

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x' 
        rev <- x$getinverse()
        if(!is.null(rev)) {
                message("getting cached data")
                return(rev)
        }
        ## get the inverse value of 'x' if has already been calculated
        mat <- x$get()
        rev <- solve(mat, ...)
        x$setinverse(rev)
        ##Calculate a matrix that is the inverse of 'x' and set it to cache 
        rev
}
