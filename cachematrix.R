## Put comments here that give an overall description of what your
## functions do


# x<-matrix (rnorm(16),ncol=4,nrow=4)
# solve(x)
# x<-makeCacheMatrix(x)
# cacheSolve(x)

## following creates an list of functions that can be used to get the original matrix,
# set (as in define) the inverse matrix and get the inverse matrix. It does not calculates the inverse 


makeCacheMatrix <- function(x = matrix()) {
      if (nrow(x) != nrow(x)){
            print ('matrix is not a squire matrix.')
      } else {
            m <- NULL
            get <- function() x # returns the original matrix
            setinverse <- function(inverse) m <<- inverse # can be used to set inverse manually
            getinverse <- function() m
            list(get = get, setinverse = setinverse, getinverse = getinverse)
            
      }
      
}



## retrieves from cache if exists. Otherwise calculates first and then retrieves

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}


