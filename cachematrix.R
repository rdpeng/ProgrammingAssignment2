makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  setm <- function(y){
    x <<- y
    m <<- NULL
  }
  getm <- function() x
  setinv <- function(z) 
    m <<- z
  getinv <- function() m
  list(setmatrix = setm, getmatrix = getm, getinv = getinv, setinv = setinv)
}

cacheSolve <- function(x,...){
  m <- x$getinv()
  if(!is.null(m)){
    print("Returning cached data")
    return(m)
  } 
  m <- solve(x$getmatrix())
  x$setinv(m)
  m
}
