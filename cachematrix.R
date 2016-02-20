## This program simply caches the inverse of a matrix when it is createdd

## Makes a matrix to cache inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
        x <<- y
        inverse <<- NULL
        }
        
        get <- function() x #gets matrix x
        setInverse <- function(inverse) inv <<- inverse #assigns inverse passed to function
        getInverse <- function() inv #gets inverse matriz
        list(set=set, get=get, setInversie = setInverse, getInverse = getInverse)
}


## Computes the Inverse Matrix (if not available already)
## If available, it should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) return(inv)
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
}
