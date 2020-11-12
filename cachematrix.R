## In these functions I created an object that stores a matrix and caches its inverse.

## The function "makeCacheMatrix" creates a list which contain a function that set the value of the matrix, get the value of the matrix,
## set the velue of the inverse and get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the matrix returned by the makeCacheMatrix function. If the inverse has already been calculated
## and it is the same the cacheSolve function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
