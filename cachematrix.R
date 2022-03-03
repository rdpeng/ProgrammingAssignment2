## The objective of the functions is to reduce the time it takes to solve the inverse of a matrix
## if the inverse of a matrix was already solved, retrieve the already solved solution
## if not, calculate the inverse of the matrix

## This function caches the inverse of a special matrix by creating a list containing separate functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inversematrix) m <<- inversematrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}