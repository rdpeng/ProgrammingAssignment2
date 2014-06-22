## Below are two functions that are used to create a special object that stores a square matrix and cache's 
## its inverse matrix.


## The first function, makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
## 1.set the value of the sqaure matrix
## 2.get the value of the sqaure matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                InverseMatrix <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) InverseMatrix <<- solve
        getinverse <- function() InverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, CacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InverseMatrix <- x$getinverse()
        if(!is.null(InverseMatrix)) {
                message("getting cached data")
                return(InverseMatrix)
        }
        data <- x$get()
        InverseMatrix <- solve(data, ...)
        x$setinverse(InverseMatrix)
        InverseMatrix
}
