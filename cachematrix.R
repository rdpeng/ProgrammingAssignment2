## makeCacheMatrix constructs an cache object which will contain matrix along with data .cacheSolve
## will find inv from either  already stored object or else by calculation .In latter case
##it will also put inv of  the matri

## Write a short comment describing this function
## set = sets x to a matrix y;
## get = returns x
## setInverse= sets inverse of x to a matrix 'inverse';
## getInverse= returns inverse of x ;
## makeCacheMatrix=  creates  a list containing above functions;




makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## calculates the inverse of the matrix in list created with the above function. However
##, it first checks to see if the inverse has already been calculated. If so, it gets the
##inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the
##matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data from environment")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
