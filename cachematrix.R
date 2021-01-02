## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## we can set the function makeCacheMatrix with input matrix value assigned to symbol "var"
## it will save in variable x and return to the caller function
## cache is set to NULL before returning from the caller function
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    setVar <- function(var = matrix()){
        x <<- var
        cache <<- NULL
    }
    getVar <- function() x
    setCacheResult <- function(result){
        cache <<- result
    }
    getCacheResult <- function() cache
    list (setVar = setVar, getVar = getVar, setCacheResult = setCacheResult, getCacheResult = getCacheResult)
}


## Write a short comment describing this function
## calling cacheSolve function by passing x = makeCacheMatrix(var) function as argument
## first find the cache whether it is stored or not
## calculate the solve() with the argument passed from x
## store the value in cache and return
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cache <- x$getCacheResult()
    if (!is.null(cache)) {
        message("getting from cache data")
        cache
    }
    my_matrix <- x$getVar()
    cache <- solve(my_matrix, ...)
    x$setCacheResult(cache)
    cache
}
