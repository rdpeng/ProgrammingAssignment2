## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates "matrix" object consisting of data (matrix and it's inverse) 
## and list of functions that can use/modify this data.
## 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates takes "matrix" (created by makeCacheMatrix) as argument. It first checks
## if inverse matrix has already been calculated (inverse is stored in "matrix"'s enviroment), if 
## so then it returns cached value. In case inverse is not cached it calculates it (and caches is)

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}


# speed comparison of cacheSolve vs solve
#
# xtest <- matrix(rnorm(100*100), 100, 100)
# xtestcache <- makeCacheMatrix(xtest)
# 
# start.time <- Sys.time()
# for (i in 1:1000) {solve(xtest)}
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# 
# start.time <- Sys.time()
# for (i in 1:1000) {suppressMessages(cacheSolve(xtestcache))}
# end.time <- Sys.time()
# time.taken.cache <- end.time - start.time
# 
# time.taken
# time.taken.cache
# rm(list = c("xtest", "xtestcache", "start.time", "end.time", "time.taken", "time.taken.cache"))
