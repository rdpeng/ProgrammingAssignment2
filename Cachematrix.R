#The following two functions are able to speed up the calculation by looking 
#for results in the cache. 

#The first function creates a list containing several functions which set the
#value of the matrix, in other words, create a matrix object. 
makeCacheMatrix <- function(x = matrix()) {         
      o <- NULL
    set <- function(y) {
          x <<- y      
          o <<- NULL   
          }
     get <- function() x  
     setinverse <- function(solve)  o <<- solve
     getinverse <- function() o
     list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


#the second function is critical in identifying whether the inverse result 
#could be taken out from the cache, otherwise, it would implement the inverse 
#action within the function itself. 

cacheSolve<- function(x, ...) {
     o <- x$getinverse()
     if(!is.null(o)) {
         message("getting cached data")
         return(o)
         }
     data <- x$get()
     o <- solve(data, ...)
     x$setinverse(o)
     o
}
