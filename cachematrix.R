makeCacheMatrix <- function(x = matrix()) {
         ## This is a special matrix that can cache its inverse 
             m <- NULL
             set <- function(y) {
                       x <<- y
                      m <<- NULL
               }
    	  get <- function() x
     	  setinv <- function(solve) m <<- solve  #use solve function to get inverse
             getinv <- function() m
     	  list(set = set, get = get, 
               	  setinv = setinv, getinv = getinv)        
     }

   
   
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
            ## The function checks if the matrix was already inverted 
            ## If so, gets the inverse from cache and skips computation
               m <- x$getinv()
               if(!is.null(m)) {
                         message("getting cached data")
                         return(m)
              }
               #if not cached, find inverse
                data <- x$get()
                 m <- solve(data, ...)
                x$setinv(m)
                 m
         }