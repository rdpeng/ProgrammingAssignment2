makeCacheMatrix <- function(x = matrix()){
  I <- NULL
   set<- function(z) {
     x <<- z
      I <<- NULL
     }
    get <- function() x
      setInverse <- function(Inverse) I <<- Inverse
    getInverse <- function() I
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
  
  }
cacheSolve <- function(x, ...){
  I <- x$getInverse()
   if(!is.null(I)){
      message("getting cached Data")
     return(I)
   }
matrix <- x$get()
    I <- solve(matrix, ...)
     x$setInverse(I)
       I

}