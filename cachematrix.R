## These functions can calculate and save the inverted matrix from an input matrix so
## that when the same inverted matrix is requested, it will be obtained from cache
## without being calculated again.

## The function `makeCacheMatrix` creates a special list, which contains a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverted matrix
## 4.  get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <<- function(y) {
            y <<- x
            v <<- NULL
        }
        
          
        get <- function() x
        setinverse <- function(solve) v <<- solve
        getinverse <- function() v
        list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

            

## The function cacheSolve is to examine if a reverse matrix is already exist in the
## environment and then either retrive it or calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}
