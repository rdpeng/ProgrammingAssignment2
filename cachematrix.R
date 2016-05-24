## Programming assignment 2 code by Eric Johnson:  eric@focusNumeric.net

## Usage:  
## > inverse <- cacheSolve(makeCacheMatrix(mat))
## The correct result can be verified using 
## > inverse %*% mat
## This will output an identity matrix (up to numerical error)
## For example:
## mat <- matrix(rnorm(81, 9, 9))   # almost certainly guaranteed to be nonsingular
## test <- makeCacheMatrix(mat)
## inverse <- cacheSolve(test)
## If this last command (line 11) is issued more than 1 time, the inverse is pulled out of cache and the message "Getting cached matrix inverse"
## is printed on the screen.

## This function takes the matrix "mat" and:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse (matrixInverse)
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(mat = matrix()) {
        matrixInverse <- NULL
        set <- function(y) {
                matrixInverse <<- y
                mat <<- NULL
        }
        get <- function() mat
        setInverse <- function(solve) matrixInverse <<- solve
        getInverse <- function() matrixInverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the matrix inverse using 'solve' if the matrix inverse has not already been calculated.  If it has, the
## calculation is skipped and result is pulled from cache.  It also sets the variable matrixInverse

cacheSolve <- function(mat, ...) {
        matrixInverse <- mat$getInverse()
        if(!is.null(matrixInverse)) {
                message("Getting cached matrix inverse")
                return(matrixInverse)
        }
        data <- mat$get()
        matrixInverse <- solve(data, ...)
        mat$setInverse(matrixInverse)
        matrixInverse
}
