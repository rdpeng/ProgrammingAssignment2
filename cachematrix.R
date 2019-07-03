## The purpose of these functions is to ceche the inverse of a matrix
## 

## This function (called makeCachMatrix) creates a matrix object which can Cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inverter) inv <<- Inverter
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv, 
       getInv = getInv)
}

##This function computes the inverse of the matrix created erlier(with the makeCacheMatrix function). It evaluates 
## if the inverse has already been calculated and then it retreves the inverse from the Cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv      
}


#checking if it works
matrix1 <- matrix(rnorm(25),5,5)

matrix2 <- makeCacheMatrix(matrix1)
cacheSolve(matrix2)


 