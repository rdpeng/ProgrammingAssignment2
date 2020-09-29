## Put comments here that give an overall description of what your
## functions do

## Two functions are involved here: makeCacheMatrix and makeCacheMatrix
## makeCacheMatrix contains set, get, setInverse and getInverse

## To make the Cache Matrix
makeCashMatrix <-function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() {x}       ##This gives the function to get matrix x
        setInverse <- function(inverse) {m <<- inverse}
        getInverse <- function() {m}  ##This is the function used to obtain the inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## To get the Cache data

cacheSolve <- function(x, ...){  ## This gets Cache data
        m <- x$getInverse()
        if(!is.null(m)){         ## Checking whether the inverse is null
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)     ##Calculates the values of the inverse
        x$setInverse(m)
        m  ##This returns a matrix that is inverse of x
}
