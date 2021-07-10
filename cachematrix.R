makeCacheMatrix <- function(x = matrix()){
        rev <- NULL
        set <- function(y){
                x <<- y
                rev <<- NULL
     }
     get <- function() {x}
     setInverse <- function(inverse) {rev <<- inverse}
     getInverse <- function() {rev}
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
     rev <- x$getInverse()
     if(!is.null(rev)){
            message("getting cached data")
            return(rev)
     }
     mat <- x$get()
     rev <- solve(mat, ...)
     x$setInverse(rev)
     rev
}
