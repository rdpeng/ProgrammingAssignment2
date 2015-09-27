## Put comments here that give an overall description of what your
#write a pair of functions that cache the inverts of a matrix

## Write a short comment describing this function
#1. makeCacheMatrix: special matrix object that caches the inverse
makeCacheMatrix <- function(x = matrix()) {
    #i. set the value of the vector
    #ii. get the value of the vector
    #iii. set the value of the mean
    #iv. get the value of the mean
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getvalue <- function() x
    setmean <- function(mean) inverse <<- mean
    getmean <- function() inverse
    list(set = set, getvalue = getvalue,
         setmean = setmean,
         getmean = getmean)
}

## Write a short comment describing this function
#2. cacheSolve: computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    #i. calculate the mean of vector created with makeVector function
    #ii. check if mean already calculated
    #iii. if already calculated, take mean from cache and skips computation
    #iv. else calculate mean of data
    #v. and sets value of mean in cache via setmean function
    inverse <- x$getmean()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data_update <- x$getvalue()
    inverse <- solve(data_update, ...)
    x$setmean(inverse)
    inverse
}
