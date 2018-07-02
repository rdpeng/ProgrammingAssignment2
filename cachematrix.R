## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Below functions (set(), get(), setinverse(), getinverse()) for creating a special matrix object 
## that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                      ## object to store inverse matrix
        set <- function(y) {
                x <<- y                                  ## using <<- operator which can be used to assign a value to an object in the parent environment
                inv <<- NULL                           
        }
        get <- function() x                              ## x is retrieved from parent environment
        setinverse <- function(inverse) inv <<- inverse  ## no value until it is called from cacheSolve
        getinverse <- function() inv                     
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")          ## if a value exists, this message will be printed
                return(inv)                             
        }
        data <- x$get()                                 ## retrieve current matrix and store it in temp variable
        inv <- solve(data, ...)                         ## calculate the inverse of the matrix in temp variable
        x$setinverse(inv)                               ## return inverted temp variable
        inv
}

## Example
# test <- makeCacheMatrix(matrix(c(2, 3, 1, 2),2,2))
# test$get()
# [,1] [,2]
# [1,]    2    1
# [2,]    3    2
# 
# cacheSolve(test)
# getting cached data
# [,1] [,2]
# [1,]    2   -1
# [2,]   -3    2
