makeVector <- function(x = numeric()) {
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
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}

## Matrix Inversion Function  
## functions to cache the inverse of matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <-function (y) {
        x <<- y
        i <<- NULL
}
get <- function () x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
        list (set = set, get = get,
        setinverse= setinverse,
        getinverse= getinverse)

## creates special matrix object that can cache its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  t <- x$getinverse()
  if(!is.null(t)) {
  message ("get cached data")
  return (t)
}
data <- x$get()
t <- solve (data)
x$setinverse(t)
 t
 }
