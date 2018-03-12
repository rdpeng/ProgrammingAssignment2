## Put comments here that give an overall description of what 
## your functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
   Inv <- NULL
   set <- function(y){                                # Set the value of the Matrix
       x <<-y
       Inv <<- NULL
   }
   get <- function() x                                # Get the value of the Matrix
   setInverse <- function(inverse) Inv <<- Inverse    # Set the value of the invertiblt Matrix
   getInverse <- function() Inv                       # Get the value of the invertible Matrix
   list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return a matrix that is the inverse of X


cacheSolve <- function(x, ...){
   Inv <- x$getInverse()
   if(!is.null(Inv)) {                               ## If inverse matrix is not NULL
        message("getting cached data")               ## Type message (getting Cached data)     
        return(Inv)                                  ## Return the invertible matrix
    }
    mat <- x$get()                                   ## Get the original Matrix Data
    Inv <- solve(mat, ...)                           ## Use solve function to inverse the matrix
    x$setInverse(Inv)                                ## Set the invertible matrix
    Inv                                              ## Return the invertible matrix
}