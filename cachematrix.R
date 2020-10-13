## ----- makeCacheMatrix ------

## This function creates a special "matrix" object that can cache its inverse.
#  Creates a list containing a function to

# 1.- set the value of the matrix
# 2.- get the value of the matrix
# 3.- set the value of the inverse
# 4.- get the value of the inverse

makeCacheMatrix <- function(Matrix = matrix()){
  Matrix <- NULL
  set <- function(matrix){ # set the value of the matrix
    x <<- y
    Matrix <<- NULL
    }
  get <- function()x # get the value of the matrix
  setinverse <- function(solve) Matrix <<- solve # set the value of the inverse
  getinverse <- function() Matrix #  get the value of the inverse
  list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}

## ----- cacheSolve ------

## This function computes the inverse of the special "matrix" returned by 
#  makeCacheMatrix above.

## If the inverse has already been calculated, 
#  the cacheSolve retrieve the inverse from the cache.

## The following function calculates the inverse of the special matrix 
#  created with the above function (makeCacheMatrix). 

## However, it first checks to see if the inverse has already been calculated. 
#  If so, it gets the inverse from the cache and skips the computation. 
#  Otherwise, it calculates the inverse of the data and sets the value of 
#  the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...){
  Matrix <- x$getinverse()
  if(!is.null(Matrix)) { # checks to see if the inverse has already been calculated
    message("getting cached data")
    return(Matrix) # gets the inverse from the cache and skips the computation
  }
  data <- x$get()
  Matrix <- solve(data, ...)
  x$setinverse(Matrix) # calculates the inverse of the data and sets the value of 
                       # the inverse in the cache via the setinverse function
  Matrix
}
