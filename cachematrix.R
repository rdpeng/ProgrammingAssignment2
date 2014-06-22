## creates a list wrapping a matrix
## with caching and set/get functionality for matrix inverses

makeCacheMatrix <- function(x = matrix()) {
    cachedData <- NULL;     ## stores inverse matrix results in closure
    set <- function(newMatrix) {
        ##clears cached result and stores new matrix on matrix reset
            x <<- newMatrix;
            cachedData <<- NULL;
    }
    get <- function(){ return(x);}
    setinverse <- function(inverse){
        #store inverse in closure and return it
        cachedData <<- inverse; return(cachedData);
    }
    getinverse <- function(){
        #return cached result if available or null
        return(cachedData);
    }
    return(list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse));
}


## determines matrix inverse:
## - if the calculation has already been made, returns the cached result
## - otherwise, calculates result and stores it in cache

cacheSolve <- function(x, ...) {
    cachedData <- x$getinverse();
    if(!is.null(cachedData)) {
            ##retrieve cached results if available
            message("retrieving cached result");
            return(cachedData);
    }
    ##if not cached, calculate matrix inverse
    data <- x$get();
    cachedData <- solve(data, ...);
    #store newly computed matrix inverse in cache
    x$setinverse(cachedData);
    return(cachedData);
}
