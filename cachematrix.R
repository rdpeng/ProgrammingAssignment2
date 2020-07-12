
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {##define the arguement with model matrix
j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
                }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j
        list(set = set,get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {## this function represents that cacheSolve is assigned by a new function, if the function works, then the result will be calculated
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
                }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}
