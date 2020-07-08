## Put comments here that give an overall description of what your
## functions do

# this function takes in matrix and create its inverse
# this matrix function will cache the inverse of a matrix insted of computing it repeatedly 

## Write a short comment describing this function
# this functioin takes any matrix and store it in object x the set value of x and y and get the inverse of this matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 # inv is an empty variable created
  set <- function(y){         # set takes an argument y and assign it to matrix x
    x <<- y                   # setting the value of the matrix
    inv <<- NULL
  }
  get <- function()x          # this is rewriting of get function, it get the value of x which has been set
  setinverse <- function(inverse) inv <<- inverse      # setting the value of inverse
  getinverse <- function() inv                         # for getting the value of inverse
  list(set = set,                                      # returns the list containing objects which have value of matrix and inverse
       get = get, 
       setinverse = setinverse,
       getinverse =getinverse)
  

}


## Write a short comment describing this function
# this function calculate inverse of the matrix created above.
# first it check for inverse. if inverse present it get inverse from cache and skip the computation
# else it calculate inverse of the matrix


cacheSolve <- function(x, ...) {
  i <- x$getinverse()               # assigning the inverse value from makeCacheMatrix function to a variable
  if(!is.null(i)){                  # checking for inverse
    message("getting cached data")  # if i is not empty means the inverse is persent and it returns i
    return(i)
  }
  matr <- x$get()                   # if inverse not present the get the data of matrix from above function
  Inv <- solve(matr, ...)           # inverse the matrix
  x$setinverse(Inv)                 # set the value of inverse matrix
  Inv                               # return the value of inverse matrix
  
}
