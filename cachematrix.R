makeCacheMatrix <- function(x = matrix()){
        cq2177 <- NULL
        set <- function(y){
                x <<- y
                cq2177 <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (cq2177 <<<- inverse}
        getInverse <- function() (cq2177)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        cq2177 <- x$getInverse()
        if(!is.null(cq2177)){
                message("getting cache data")
                return(cq2177)
        }
        mat <- x$get()
        cq2177 <- solve(mat, ...)
        x$setInverse(cq2177)
        cq2177
}
        
