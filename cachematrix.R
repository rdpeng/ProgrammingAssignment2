## Assignment 2
## Caching the Inverse of a Matrix

## Function creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function computes the inverse of the special 'matrix' returned by makeCacheMatrix abobe.
cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Sample run:
## > x <- rbind(c(1, -0.5), c(-0.5, 1))
## > m <- makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## Retrieve from the cache
## > cacheSolve(m)
## getting cached data
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333