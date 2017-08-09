## Function that caches a matrix and its inverse and return these information in a list.

makeCacheMatrix <- function(x = matrix()) {
    
    mInv <- NULL
    set <- function(m) {
        
        x <<- m
        mInv <<- NULL
        
    }
    
    get <- function() x
    
    setInverse <- function(inv) {
        
        mInv <<- inv
        
    }
    
    getInverse <- function() mInv
    
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that returns the inverse of a matrix. If it has already been calculated, the matrix in cache will be returned. Otherwise, the function 'solve' will be called to generate the inverse.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInverse()
    
    if(!is.null(mInv)) {
        message("getting cache data")
        return(mInv)
    }
    
    mCache <- solve(x$get())
    x$setInverse(mCache)
    mCache
    
}
