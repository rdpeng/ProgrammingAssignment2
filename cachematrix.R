## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y){
##Use "<<-" to assign value to an object in a different environment    
}
get = function() x
setinv = function(inverse)inv <<-inverse
getinv = function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    ## if the inverse has already been calculated
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    #Otherwise calcuate the inverse
    mat.data = x$get()
    inv = solve(mat.data,...)
    #sets the value of the inverse in the cache via the setinv function
    x$setinv(inv)
    return(inv)
}
