## A function that creates a special "matrix" object that can cache its inverse and 
## a function that computes the inverse of the special "matrix" returned by makeCacheMatrix, however,
## if the inverse has already been calculated (and the matrix has not changed), the function 
## should retrieve the inverse from the cache. 

## First function which creates a special object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
        
        # Initialize a square invertible matrix
        m1 <- NULL
        
        # set the matrix
        set <- function(y) {
                # the use of '<<-' is to assigne a value to an object in 
                # an environment different fron the current environment
                m <<- y
                m1 <<- NULL
        }
        
        # get the matrix
        get <- function() {
                m
        }
        
        # set the inverse of matrix 'm' 
        setInverse <- function(inverse) {
                m1 <<- inverse
        }
        
        # get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                m1
        }
        
        ## Return a list of the methods which are use as the input to cacheSolve()
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Second function that computes the inverse of the special "matrix" returned by makeCacheMatrix, however,
## if the inverse has already been calculated (and the matrix has not changed), the function 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## x which is the output of makeCacheMatrix()
        m1 <- x$getInverse()
        
        ## if the inverse has already been calculated
        if(!is.null(m1)) {
                message("getting cached data")
                return(m1)
        }
        
        ## matrix has changed, calculate the inverse
        data <- x$get()
        m1 <- solve(data)
        x$setInverse(m1)
        m1
}


## test sample to verify funcionality

## x = rbind(c(3, -1/5), c(-1/5, 3))

## m = makeCacheMatrix(x)

## m$get()
##       [,1] [,2]
## [1,]  3.0 -0.2
## [2,] -0.2  3.0

## cacheSolve(m)
##            [,1]       [,2]
## [1,] 0.33482143 0.02232143
## [2,] 0.02232143 0.33482143

## cacheSolve(m)
## getting cached data
##            [,1]       [,2]
## [1,] 0.33482143 0.02232143
## [2,] 0.02232143 0.33482143