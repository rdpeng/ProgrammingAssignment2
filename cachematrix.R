## makeCacheMatrix is for creating a special matrix
## cacheSolve is meant for calculating inverse of special matrix created by makeCacheMatrix

## Defnition of special Matrix created

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## As mentioned in assignment, it's assumed that the matrices created are invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
         }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

##Example to call these functions->
# mymat<-makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(mymat)
