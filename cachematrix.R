## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse object
  i <- NULL
  # set the new matrix value 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the matrix
  get <- function() x
  # set inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  # get the inverse matrix
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  # return inverse if already available in cached data
  if(!is.null(i)) {
    message("getting cached data")
    return(m)
  }
  # get matrix from data
  data <- x$get()
  #  calculate the inverse
  i <- solve(data)
  # set the new inverse and print inverse value
  x$setInverse(i)
  i
}


a1 <- c(3, 2, 5)
a2 <- c(2, 3, 2)
a3 <- c(5, 2, 4)
A <- rbind(a1, a2, a3)
# print the original matrix
print(A)
# Use the solve() function to calculate the inverse.
T1 <- solve(A)
# print the inverse of the matrix.
print(T1)

test <- makeCacheMatrix(A)
test$get()
test$getInverse()
cacheSolve(test)
test$getInverse()
