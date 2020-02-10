## Functions below create a matrix and caches its inverse


## First function, makeCacheMatrix creates a special matrix which contains a function to:

##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y)      {
            x<<-y
            m<<-NULL
      }
      
      get <- function() x
      
      setinverse<-function(inverse) m<<-inverse
      getinverse<-function() m
      list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
      
}

## The following function calculates the inverse of the matrix created with the above function. It first checks to 
## if the inverse has been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise,
## calculates invervese of matrix


cacheSolve <- function(x, ...) {
      m <-x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data,...)
      x$setinverse(m)
      m
}
