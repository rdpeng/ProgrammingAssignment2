# makeCacheMatrix 
# this function should create a special matrix object
makeCacheMatrix <- function(x = matrix()){ # creating an empty matrix
  m <- NULL # emptying any previously created matrices
  set <- function(y){ 
    x <<- y # setting y equal to x in the parent environment
    m <<- NULL # setting m in the parent environment to NULL
  }
  get <- function() x # Simply asigning the value of x in the parent environment to get
  setInverse <- function() m <<- solve(x) # basically a command to calculate the inverse of x
  getInverse <- function() m # printing the value of m in the parent environment
  list(set = set, get = get, setInverse = setInverse, getInverse = getInvers) # making a list and asigning those names to the values
}

# cacheSolve
# this function first checks if the inverse of the above created matrix exists
# if the inverse already exists, then the function retrieves the inverse from the cache
# otherwise, it computes the inverse and stores it in the cache
cacheSolve <- function(x, ...){
  m <- x$getInverse() # getting the inversed matrix and assigning it to m
  if(!is.null(m)){ # checking if any previously inversed matrix exists and returning it if yes
    message("getting cached data")
    return(m)
  } else{data <- x$get() # if no previously inversed matrix exists, x will be retrieved and assigned to data
  m <- solve(data,...) # calculating the inverse of a matrix and assigning it to m
  x$setInverse(m) # the inversed matrix will be set
  m # the inversed matrix will be returned
  }
}
