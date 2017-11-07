##These functions are intended to cache
##previous results of matrix sersing &
##take them from the cache if the matrix 
##input was previously sersed

##This function is to create a function
##list

makeCacheMatrix <- function(x = matrix()) {
                s <- NULL
                
                #func_1: set the original inputs
                
                set <- function(y) {
                  x <<- y      
                  s <<- NULL
                }
                
                #func_2 catch & return the matrix input
                
                get <- function() x
                
                #func_3 set the serse action
                
                setsolve <- function(solve) s <<- solve
                
                #func_4 get the result of sersing
                
                getsolve <- function() s
                
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
                
  
}

##This function is to judge  whether the serse has been
##calculated & return the serse value accordingly

cacheSolve <- function(x, ...) {
          s <- x$getsolve()
  
          if(!is.null(s)){
            message("getting cached data")
            return(s)
          }
  
          data <- x$get()
          s <- solve(data, ...)
          x$setsolve(s)
          s
  
}
