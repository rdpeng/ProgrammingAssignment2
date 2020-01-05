## Firts Function
## MakeCacheMatrix

## This function creates a special "matriz" object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
    
  i <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
   get <- function() {
       m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
    getInverse <- function() {
        i
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#Second Function:cacheSolve 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
   m <- solve(data) %*% data
  
   x$setInverse(m)
  
  m
}

x<-rbind (c(1, -1/16),c(-1/16,1))
m<-makeCacheMatrix(x)
m$get()

#Example
        [,1]    [,2]
[1,]  1.0000 -0.0625
[2,] -0.0625  1.0000
cacheSolve(m)
     [,1] [,2]
[1,]    1    0
[2,]    0    1
cacheSolve(m)
getting cached data
     [,1] [,2]
[1,]    1    0
[2,]    0    1
  
