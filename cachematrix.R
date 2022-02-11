#function that store inversible matrix cache.
makeCacheMatrix <- function(x = matrix()) {
  #declare i as a null object to initalize. 
  i <- NULL
  #set the matrix by storing on 'set' variable. 
  set <- function(matrix){
    #variable i & m should assign as a superassignment operator in order to access outer functions. 
    m <<- matrix
    i <<- NULL
  }
  #get the matrix
  get <- function() {
    m
  }
  #set the inverse matrix
  setinv <- function(inverse) {
    i <<- inverse
  }
  getinv <- function() {
    #get and return the inverse property
    i
  }
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  #in case of already computed data
  if (!is.null(m)) {
    message("this is cached data")
    return(m)
  }
  data <- x$get()
  #inverse object
  m <- solve(data) %*% data
  x$setinv(m)
  #return
  m
  
}
