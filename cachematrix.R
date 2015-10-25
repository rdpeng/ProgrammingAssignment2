## Put comments here that give an overall description of what your
## functions do

## The first function creates a list of 4 functions:
## 1. set - stores a matrix
## 2. get - calls/prints the matrix from memory
## 3. setinverse - stores the inverse of the matrix
## 4. getinverse - calls/prints the inverse

makeCacheMatrix <- function(x = matrix()) {
        i1 <- NULL
        set <- function(y) {
                x <<- y
                i1 <<-NULL
        }
        get <- function() x
        setinverse <- function(inv) i1 <<- inv
        getinverse <- function() i1
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## This function is used for calculating the inverse of the matrix
## It also checks whether the inverse was already stored
## by 'makeCacheMatrix".
## If true, it simply prints it out

cacheSolve <- function(x, ...) {
        i2 <- x$getinverse()
        if(!is.null(i2)) {
                message("getting cached matrix")
                return(i2)
        }
        data <- x$get()
        i2 <- solve(data, ...)
        x$setinverse(i2)
        i2
}
