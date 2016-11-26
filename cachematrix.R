## The second programming assignment in the Coursera Data Science Toolbox Specialization
## - R Programming course. 
##
## Created by BCGuyatt - Nov 2016
##
## Aim is to write two functions which allows the inverse of a matrix to be
## calculated and saved in cache, rather than recomputed each time the
## funtion is called.

## makeCacheMatrix function creates a special matrix which is a list of containing 
## a function to 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the value of the inverse of the matrix, and 4) get the value of the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){x <<- y; i <<- NULL}
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function returns the inverse of the matrix. It first checks to see if the 
## inverse has been calculated and if so, returns this value and if not then it calculates
## the value of the inverse and returns it.

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)){message("Getting the cached value"); return(i)}
        m <- x$get()
        i <- solve(m)
        x$setinverse(i)
        return(i)
}


# My example output
# > x <- matrix(c(1,2,3,4),2,2)
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m <- makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# Getting the cached value
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
