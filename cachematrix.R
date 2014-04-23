## Matix inversion
## Calculates fresh if not cached and stores values 
## Takes from cache, if already claculated and stored

## Creates a special vector, as list, containing functions
## for setting and getting values of inverse of matrix

makecachematrix<-makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y){
    x<<-y
    m<<-NULL
}
get <- function() x
setinv<- function(solve) m <<- solve 
getinv<-function() m
## Write a short comment describing this function
list( set = set, get = get,
      setinv = setinv,
      getinv = getinv)
}
## Actual inversion of matrix done 
## Solved or taken from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        m<- x$getinv()
    if(!is.null(m)){
      message("Not calculating and getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  }
