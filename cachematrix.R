

##The first function, makeVector creates a special "vector", which is really a list containing a function to

##set the value of the vector

##get the value of the vector

##set the value of the mean

##get the value of the mean


makeCacheMatrix <- function(x=matrix()){
    inv <-NULL
    set <- function(y){
        x <<- y
        inv <<-NULL
    }
    get <- function() {x}
    setInverse <- function (inverse) {inv<<- inverse}
    getInverse <- function () {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
}

##The following function calculates the mean of the special "vector" created with 
##the above function. However, it first checks to see if the mean has already 
##been calculated. If so, it gets the mean from the cache and skips 
##the computation. Otherwise, it calculates the mean of the data and sets the 
##value of the mean in the cache via the setmean function.

cacheSolve <- function (x,...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}