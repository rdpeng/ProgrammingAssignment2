## Two functions are defined as below:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##	If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## Function mackCacheMatrix creates a special "matrix", which has some functions
## - to set the base matrix of special "matrix", note related inverse is set as empty
## - to get the base matrix
## - to set the inverse of the base matrix
## - to get the inverse of the base matrix

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) xInv <<- inv
        getInverse <- function() xInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Function cacheSolve computes the inverse of one special "matrix" defined by makeCacheMatrix.
## The cached inverse is returned if available.
## Otherwise the inverse of the base matrix is calculated and cached with the special "matrix".

cacheSolve <- function(x, ...) {
        xInv <- x$getInverse()
        if(!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
        }
        data <- x$get()
        xInv <- solve(data, ...)
        x$setInverse(xInv)
        xInv
}

# To test the example
# step1: generate one base matrix
# test <- matrix(c(1,8,2,9,15,11,17,18,21,29,23,21,35,39,30,38), nc = 4)

# step2: use makeCacheMatrix and cacheSolve to create data for new matrix test
# compare the inverse in special 'matrix' with the real inverse of test
# testMatrix <- makeCacheMatrix(test)
# testInv <- cacheSolve(testMatrix)
# testInv == solve(test)

# step3: get reverse of test again through cacheSolve, and the result is fetched from cached data
# cacheSolve(testMatrix)

# step4: try to change base matrix of special 'matrix', then the inverse is removed
# testMatrix$getInverse()
# testMatrix$set(test+1)
# the inverse is removed if the base matrix is set with previous command
# testMatrix$getInverse()
# cacheSolve(testMatrix)
# testMatrix$getInverse()
