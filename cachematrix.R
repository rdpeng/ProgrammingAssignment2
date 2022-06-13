
##Makes matrix
makeCacheMatrix <- function( m = matrix()) {
  i <-NULL
  
  set <- function(matrix) {
      m <<- matrix
      i <<- NULL
  }
  
  ###Returns matrix
  get <- function() {
      m
  }
  
  ###Inverse now
  setInverse <- function(inverse) {
      i <<- inverse
  }
  
  getInverse <- function(){
      i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x,...) {
  m<- x$getInverse()
  
  if (!is.null(m)) {
      return (m)
    
  c <- x$get()
  
  m<-solve(c) %% c
  
  x$setInverse(m)
  
  m
  }
}
