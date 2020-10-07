makeCachematrix <- function(x = matrix()){
      inverse <- NULL
      set <- function(y){
             x <<- y
             inverse <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inverse <<- inverse}
      getInverse <- function() {inverse}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
      inverse <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inverse)
      }
      mat <- x$get()
      inverse <- solve(mat, ...)
      x$setInverse(inverse)
      inverse
}
