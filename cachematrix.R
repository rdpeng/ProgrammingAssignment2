## Put comments here that give an overall description of what your
#write a pair of functions that cache the inverts of a matrix

## functions do

## Write a short comment describing this function
#1. makeCacheMatrix <- create special matrix object that caches the inverse
makeCacheMatrix <- function(x = matrix()) {
makeVector <- function(x = numeric()) {
#i. set the value of the vector
#ii. get the value of the vector
#iii. set the value of the mean
#iv. get the value of the mean
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
#i. calculate the mean of vector created with makeVector function
#ii. check if mean already calculated
#iii. if already calculated, take mean from cache and skips computation
#iv. else calculate mean of data
#v. and sets value of mean in cache via setmean function
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
}

## Write a short comment describing this function
#2. cacheSolve: computes the inverse of the special matrix returned by makeCacheMatrix

#if inverse calculated (no change in matrix), then cachesolve should retrieve the inverse of thde cache
#inverse of a square matrix **solve function
#if X is a square invertible matrix, then solve(x) returns its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
