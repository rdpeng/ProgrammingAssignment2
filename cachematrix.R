## Put comments here that give an overall description of what your
## These functions return the inverse of the matrix, controled by the cached value of this result

######### makeCacheMatrix #########
## create four sub function (set, get, setmatrix, getmatrix)
## to cache the matrix in a different environment of the Global Environment
## this process needs to uso the operator <<- 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

######### cacheSolve #########
## create a function to inverse the matrix and save in the makeCacheMatrix
## step 1 - check if there is a matrix cached in another environment
## step 2 - if there is a matrix cached, return the cached value
##          if there is no a matrix cached, use the solve function to calculate the inverse matrix
##          and save whit setmaxtrix (m)... after this, return the value of inverse matrix

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setmatrix(m)
        m
}
