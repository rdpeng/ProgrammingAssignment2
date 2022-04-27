#Second programming assignment:

#*makeCacheMatrix:* This function creates a special "matrix" object that can 
#                   cache its inverse.

#*cacheSolve:* This function computes the inverse of the special "matrix" returned
#              by `makeCacheMatrix` above. If the inverse has already been calculated
#              (and the matrix has not changed), then `cacheSolve` should retrieve 
#              the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) { 
  
                  i <- NULL              #where the inverse is to be stored
                  set <- function(y) {   
                  x <<- y                #The <<- operator is necessary to enclose the enviroment of the parent function 
                  i <<- NULL
          }                       
  
     get <- function() x     #Anonymus functions to set and get a matrix and cahce its inverse
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


# Now, we are interested in calculating the inverse of the matrix object generated 
#by the parent function makeCacheMatrix. 
#If the inverse is created (!is.null) then cacheSolve will retrieve it, otherwise 
#it will calculate it. 
 
cacheSolve <- function(x, ...) {
       
                 i <- x$getinverse()
                 if (!is.null(i)) {
                 message("getting cached data")
                 return(i)
          }
                 
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
  
}
