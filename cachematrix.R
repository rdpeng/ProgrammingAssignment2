## creating function which takes argument as matrix
## It contains get,set,getinverse,setinverse funcions

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  
  ##getter and setter for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ##getter and setter for matrix inverse
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## listing the values 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function takes argument x 
## this is for calculating inverse of matrix


cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ##this will return the value if already calculated
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## computing inverse of matrix
  data <- x$get()
  
  ## solve is a function to find inverse
  
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## taking example to check the program
## 2 x 2 matrix

matrix1 <- matrix(1:4, nrow = 2, ncol = 2)
matrix1

## matrix1 is the value in makecachematrix 

matrix2 <- makecachematrix(matrix1)

## this will print inverse of a matrix

cachesolve(matrix2)
