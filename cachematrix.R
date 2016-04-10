## Set the value of the martix
## Get the value of the Matrix
## Set the inverse of the matrix
## Get the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(,nrow , ncol)) {
i <<- NULL
set <- function (y) {
    x <<- y
    i <<- NULL}
get <- function () x
setinverse <- function (solve) s <<- inverse
getinverse <- function () s
list(set = set, get = get,
     setinverse = setinverse ,
     getinverse  = getinverse )  
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache. 

    cacheSolve <- function(x, ... ) {        
        s <- x$getinverse ()
        if(!is.null(s)) { 
            message ("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
    }
