###  Assignment2 - Write 2 functions that cache the inverse of a matrix.
###   1. makeCacheMatrix()
###   2. cacheSolve()
 
source("cachematrix.R")
 
 ### 1. This function creates a special "matrix" object that can cache its inverse. 
   makeCacheMatrix <- fuction(amatrix...)
  {
   	  amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow =2 , ncol = 2))
	  amatrix$get()                    ## return original matrix
 	 amatrix$getinverse()      ## return matrix inverse
    }


### 2. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
### If the inverse has already been calculated then cacheSolve retrieves the inverse from cache.  
  
cacheSolve <- function(amatrix ...) {
         
            m <-  cachemean(amatrix)
            if(!is.null(m)) {
                      message (" getting cached data" )
                      return(m)
             }
            data <- amatrix$get()
            m <- solve(data, ...)
            amatrix$setmean()
            m
      }
   
}
