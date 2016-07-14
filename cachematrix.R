## makeCacheMatrix is for creating a special matrix
## cacheSolve is meant for calculating inverse of special matrix created by makeCacheMatrix

## Defnition of special Matrix 
# Here, we are creating a special matrix which has an internal variable that stores its inverse.
# It has accessors to retrieve/set data & inverse.

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
# cacheSolve is for calculating inverse of special matrix created by above method.
# But it first checks that if inverse for the matrix created exists, 
# then it retrieves & displays else it calculates & sets this inverse in matrix's internal variable
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        # check if inverse already exists
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
         }
  # else calculate the inverse & set it
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

##Example to call these functions->
# mymat<-makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(mymat)
