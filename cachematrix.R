## My funcitons will effectively cache and print the inverse of
## a matrix.  This function assumes the matrix is always invertible.

## makeCacheMatrix creates a list containing a function to
## get and set the value of a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
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

## cacheSolve will return a matrix that is the inverse of 'x'.
## First, it will see if the inverse has already been computed.
## if it has, the function will print that value.
## If it has not, the function will compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

## Function Test Results:

## > a <- matrix(1.5:4.5, nrow = 2)

## > a
##      [,1] [,2]
## [1,]  1.5  3.5
## [2,]  2.5  4.5

## > b <- makeCacheMatrix(a)

## > b$get()
##      [,1] [,2]
## [1,]  1.5  3.5
## [2,]  2.5  4.5

## > cacheSolve(b)
##       [,1]  [,2]
## [1,] -2.25  1.75
## [2,]  1.25 -0.75

## > cacheSolve(b)
## getting cached data
##       [,1]  [,2]
## [1,] -2.25  1.75
## [2,]  1.25 -0.75
