makeCacheMatrix <- function(x = matrix()){
   Inv <- NULL
   set <- function(y){
       x <<-y
       Inv <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) Inv <<- Inverse
   getInverse <- function() Inv
   list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




cacheSolve <- function(x, ...){
   Inv <- x$getInverse()
   if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    mat <- x$get()
    Inv <- solve(mat, ...)
    x$setInverse(Inv)
    Inv
}