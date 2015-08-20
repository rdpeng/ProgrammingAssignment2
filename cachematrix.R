## This function creates a list of functions to set and get 
## a given matrix and it's inverse

##

makeCacheMatrix <- function(x=matrix()){
  # set the initial value of i to NULL
  # in this variable is where the inverse is going to be stored
  i <- NULL
  
  # store the value of the matrix in the variable x
  # both x and y are set in the environment
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  
  # retrives the value of the matrix
  get <- function() m
  
  # store the value of the matrix inverse in i
  setinv <- function(inv) i <<- inv
  
  # retrieves the value of the matrix inverse
  getinv <- function() i
  
  # set a named list whose elements are the functions above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks and returns the value of the matrix inverse
## if it's in the environment, otherwise it calculates and return it's value

cacheSolve <- function(x,...) {
  # retrieves the data in the 'getinv' variable
  i <- x$getinv()
  
  # if it's not NULL returns the value in i
  if(!is.null(i)) {
    message("getting cached data")
    return(m)
  }
  
  
  # otherwise, retrieves the data in the 'get' variable
  data <- x$get()
  
  # calculate the matrix inverse
  message("calculating the matrix inverse")
  i <- solve(data)
  
  # saves the value of i 
  x$setinv(i)
  
  # returns the matrix inverse
  i
}
