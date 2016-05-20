#two functions jointly compute the inverse of new matrix and cache it 
#or print the inverse out if the results have been stored in cache

#create a special "matrix" object that can cache its inverse
#parent function controls the whole process while child function does the work

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    setM <- function(y) {
        x <<- y        #double arrow operator in child function
        s <<- NULL
    }
    getM <- function() x
    setInv <- function(inv) s <<- inv
    getInv <- function() s
    list(setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}

#computes the inverse of the special "matrix" returned by makeCacheMatrix
#if the inverse has already been calculated and the matrix unchanged
#retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    s <- x$getInv()
    if(!is.null(s)) {
        message("cache retrieved")
        return(s)
    }
    data <- x$getM()
    s <- solve(data)
    x$setInv(s)
    s
}
