########## INVERSE ###############

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  mat <- x
  
  # creating the set function of the given matrix
  set <- function(y) {
    mat <<- y
    inv <<- NULL    
  }
  
  ## defining the get, setinverse, getinverse function
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## defining the list of function in makeCacheMatrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


cacheSolve <- function(x, ...){
  ## defining the output of the function getinverse() as INV
  INV <- x$getinverse()
  
  ## checking for the existence of INV
  if(is.null(INV)){
    message("calculating data (no cache)")
  } else {
    message("getting cached data")
  }
  
  ## defining the output of get() as input data and invert the matrix
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## defining input matrix xx
xx <- rbind(c(1,2,4), c(2,1,8), c(64,96,11))


## getting cached data
inv1 <- makeCacheMatrix(xx)
inv1$setinverse(xx)
cacheSolve(inv1)



## data is calculated (not cached)
cacheSolve(makeCacheMatrix(xx))

