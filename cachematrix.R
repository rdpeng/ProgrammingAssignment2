## Like the example provided, this function completes 4 tasks:

## 1. it sets the value of the matrix
## 2. it gets the value of the matrix
## 3. it sets the value of the inverse of the matrix
## 4. it gets the value of the inverse - ta da!

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


## Next, the following function calculates the inverse of the matrix created in the first function. First, it checks to see if the inverse has been calculated. If it has, it retrieves the inverse value from the cache, and skips the calculation. If the inverse has not been calculated, the function proceeds and sets the vlaue of the matrix in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        		message("retrieving cached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        #retuns inverse matrix o 'x'
}


